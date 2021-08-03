using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using Grasshopper.Kernel;
using Rhino.Geometry;

namespace wfc_gh {
    public class GH_WaveFunctionCollapse3D : GH_Component {

        private const int IN_PARAM_RULE_AXIS = 0;
        private const int IN_PARAM_RULE_LOW = 1;
        private const int IN_PARAM_RULE_HIGH = 2;
        private const int IN_PARAM_WORLD_SIZE = 3;
        private const int IN_PARAM_WORLD_SLOT_POSITION = 4;
        private const int IN_PARAM_WORLD_SLOT_MODULE = 5;
        private const int IN_PARAM_RANDOM_SEED = 6;
        private const int IN_PARAM_MAX_ATTEMPTS = 7;

        private const int OUT_PARAM_DEBUG_OUTPUT = 0;
        private const int OUT_PARAM_WORLD_SLOT_POSITION = 1;
        private const int OUT_PARAM_WORLD_SLOT_MODULE = 2;

        private readonly System.Drawing.Bitmap componentIcon;

        public GH_WaveFunctionCollapse3D() : base("Wave Function Collapse 3D",
                                                  "WFC3",
                                                  "Solver for the Wave Function Collapse algorithm in cubic voxel space",
                                                  "WaveFunctionCollapse",
                                                  "WaveFunctionCollapse") {
            using (var ms = new MemoryStream(Resources.ComponentIcon)) {
                componentIcon = new System.Drawing.Bitmap(ms);
            }
        }

        public override Guid ComponentGuid => new Guid("05d6620c-8bb7-44f0-8375-a8120ee0c9b2");

        protected override System.Drawing.Bitmap Icon => componentIcon;

        protected override void RegisterInputParams(GH_InputParamManager pManager) {
            pManager.AddTextParameter("Rule axis",
                                      "RA",
                                      "A list of string axes (\"X\", \"Y\" or \"Z\") definitions along which to apply the rule. Zipped with RL and RH.",
                                      GH_ParamAccess.list);
            pManager.AddTextParameter("Rule low",
                                      "RL",
                                      "A list of string identifiers of the module on the low dimension along the axis. Zipped with RA and RH.",
                                      GH_ParamAccess.list);
            pManager.AddTextParameter("Rule high",
                                      "RH",
                                      "A list of string identifiers of the module on the high dimension along the axis. Zipped with RA and RL",
                                      GH_ParamAccess.list);

            pManager.AddVectorParameter("World size",
                                        "WS",
                                        "A vector defining the X, Y and Z dimensions of the world. Real numbers in the vector will be rounded to the nearest integer and must be positive (they define size).",
                                        GH_ParamAccess.item,
                                        new Vector3d(5.0, 5.0, 5.0));

            pManager.AddVectorParameter("Slot positions",
                                        "SP",
                                        "A list of vectors defining positions of slots that will be contain values from SM. Zipped with SM. A position can occur multiple times in this list, each adding the correspoding SM to the slot. Real numbers in the vectors will be rounded to the nearest integer and must be nonnegative (they define indices)",
                                        GH_ParamAccess.list);

            pManager.AddTextParameter("Slot modules",
                                      "SM",
                                      "A list of string identifiers defining modules to add to slots at position from SP. Zipped with SP. A module can occur multiple times in this list, each adding the module at the corresponging SP position.",
                                      GH_ParamAccess.list);

            pManager.AddIntegerParameter("Random seed",
                                         "S",
                                         "Random number generator seed",
                                         GH_ParamAccess.item,
                                         42);
            pManager.AddIntegerParameter("Max attempts",
                                         "A",
                                         "Maximum number of tries before the solver gives up",
                                         GH_ParamAccess.item,
                                         100);
        }

        protected override void RegisterOutputParams(GH_OutputParamManager pManager) {
            pManager.AddTextParameter("out", "out", "Debug output and metrics", GH_ParamAccess.item);
            pManager.AddVectorParameter("Slot positions",
                                        "SP",
                                        "Positions of the state of the world once the wave function has collapsed. Has exactly the same format as the SP input parameter.",
                                        GH_ParamAccess.list);
            pManager.AddTextParameter("Slot modules",
                                      "SM",
                                      "Modules of the state of the world once the wave function has collapsed. Has exactly the same format as the SM input parameter.",
                                      GH_ParamAccess.list);
        }

        protected override void SolveInstance(IGH_DataAccess DA) {
            var stats = new Stats();
            var stopwatch = new Stopwatch();

            stopwatch.Reset();
            stopwatch.Start();

            //
            // -- Adjacency rules --
            //
            // The rules come in three lists of the same length. The first contains texts
            // representing the axis/kind (x/y/z).
            // Second and third list contain unique textual identifiers of the modules.
            // This importer replaces those string names with generated u16 numbers,
            // starting with 0.

            var adjacencyRulesAxis = new List<string>();
            var adjacencyRulesModuleLow = new List<string>();
            var adjacencyRulesModuleHigh = new List<string>();
            DA.GetDataList(IN_PARAM_RULE_AXIS, adjacencyRulesAxis);
            DA.GetDataList(IN_PARAM_RULE_LOW, adjacencyRulesModuleLow);
            DA.GetDataList(IN_PARAM_RULE_HIGH, adjacencyRulesModuleHigh);

            int adjacencyRulesMinCount = Math.Min(Math.Min(adjacencyRulesAxis.Count,
                                                           adjacencyRulesModuleLow.Count),
                                                  adjacencyRulesModuleHigh.Count);

            if (adjacencyRulesMinCount <= 0) {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "Must supply at least one adjacency rule");
                return;
            }

            // Check ahead of time, if there are at most maxModuleCount modules
            // altogether in the input.
            uint maxModuleCount = Native.wfc_query_max_module_count();
            {
                HashSet<string> allModules = new HashSet<string>();

                for (var i = 0; i < adjacencyRulesMinCount; ++i) {
                    allModules.Add(adjacencyRulesModuleLow[i]);
                    allModules.Add(adjacencyRulesModuleHigh[i]);
                }

                if (allModules.Count > maxModuleCount) {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Too many modules. Maximum allowed is " + maxModuleCount);
                    return;
                }
            }

            ushort nextModule = 0;
            var nameToModule = new Dictionary<string, ushort>();
            var moduleToName = new Dictionary<ushort, string>();
            var adjacencyRules = new AdjacencyRule[adjacencyRulesMinCount];

            for (var i = 0; i < adjacencyRulesMinCount; ++i) {
                string axisStr = adjacencyRulesAxis[i];
                string lowStr = adjacencyRulesModuleLow[i];
                string highStr = adjacencyRulesModuleHigh[i];

                AdjacencyRuleKind kind;
                switch (axisStr) {
                    case "x":
                    case "X":
                        kind = AdjacencyRuleKind.X;
                        break;
                    case "y":
                    case "Y":
                        kind = AdjacencyRuleKind.Y;
                        break;
                    case "z":
                    case "Z":
                        kind = AdjacencyRuleKind.Z;
                        break;
                    default:
                        DA.SetDataList("World state", null);
                        return;
                }

                ushort low = 0;
                if (nameToModule.ContainsKey(lowStr)) {
                    nameToModule.TryGetValue(lowStr, out low);
                } else {
                    low = nextModule;
                    nameToModule.Add(lowStr, low);
                    moduleToName.Add(low, lowStr);
                    nextModule++;
                    Debug.Assert(nextModule < maxModuleCount);
                }

                ushort high = 0;
                if (nameToModule.ContainsKey(highStr)) {
                    nameToModule.TryGetValue(highStr, out high);
                } else {
                    high = nextModule;
                    nameToModule.Add(highStr, high);
                    moduleToName.Add(high, highStr);
                    nextModule++;
                    Debug.Assert(nextModule < maxModuleCount);
                }

                AdjacencyRule rule;
                rule.kind = kind;
                rule.module_low = low;
                rule.module_high = high;
                adjacencyRules[i] = rule;
            }

            //
            // -- World dimensions --
            //

            var worldSize = Vector3d.Zero;
            DA.GetData(IN_PARAM_WORLD_SIZE, ref worldSize);

            int worldXInt = (int)Math.Round(worldSize.X);
            int worldYInt = (int)Math.Round(worldSize.Y);
            int worldZInt = (int)Math.Round(worldSize.Z);

            if (worldXInt <= 0) {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World X must be a positive integer");
                return;
            }
            if (worldYInt <= 0) {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World Y must be a positive integer");
                return;
            }
            if (worldZInt <= 0) {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World Z must be a positive integer");
                return;
            }

            ushort worldX = (ushort)worldXInt;
            ushort worldY = (ushort)worldYInt;
            ushort worldZ = (ushort)worldZInt;

            //
            // -- World slot positions and modules --
            //

            // Note: These lists will be cleared and re-used for output.
            var worldSlotPositions = new List<Vector3d>();
            var worldSlotModules = new List<string>();

            DA.GetDataList(IN_PARAM_WORLD_SLOT_POSITION, worldSlotPositions);
            DA.GetDataList(IN_PARAM_WORLD_SLOT_MODULE, worldSlotModules);

            int worldSlotMinCount = Math.Min(worldSlotPositions.Count, worldSlotModules.Count);

            //
            // -- Random seed --
            //
            // wfc_rng_state_init needs 128 bits worth of random seed, but that
            // is tricky to provide from GH.  We let GH provide an int, use it
            // to seed a C# Random, get 16 bytes of data from that and copy
            // those into two u64's.

            int randomSeed = 0;
            DA.GetData(IN_PARAM_RANDOM_SEED, ref randomSeed);

            var random = new Random(randomSeed);
            byte[] rngSeedLowArr = new byte[8];
            byte[] rngSeedHighArr = new byte[8];
            random.NextBytes(rngSeedLowArr);
            random.NextBytes(rngSeedHighArr);

            if (!BitConverter.IsLittleEndian) {
                // If we are running on a BE machine, we need to reverse the bytes,
                // because low and high are defined to always be LE.
                Array.Reverse(rngSeedLowArr);
                Array.Reverse(rngSeedHighArr);
            }

            ulong rngSeedLow = BitConverter.ToUInt64(rngSeedLowArr, 0);
            ulong rngSeedHigh = BitConverter.ToUInt64(rngSeedHighArr, 0);

            //
            // -- Max attempts --
            //

            int maxAttempts = 0;
            DA.GetData(IN_PARAM_MAX_ATTEMPTS, ref maxAttempts);

            //
            // -- Initialize world state and feed slot data --
            //

            var wfcWorldStateHandle = IntPtr.Zero;
            var wfcWorldStateHandleBackup = IntPtr.Zero;
            var wfcRngStateHandle = IntPtr.Zero;
            unsafe {
                fixed (AdjacencyRule* adjacencyRulesPtr = &adjacencyRules[0]) {
                    var result = Native.wfc_world_state_init(&wfcWorldStateHandle,
                                                             adjacencyRulesPtr,
                                                             (UIntPtr)adjacencyRules.Length,
                                                             worldX,
                                                             worldY,
                                                             worldZ,
                                                             0);

                    switch (result) {
                        case WfcWorldStateInitResult.Ok:
                            break;
                        case WfcWorldStateInitResult.ErrModuleCountTooHigh:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: Adjacency rules contained too many modules");
                            return;
                        case WfcWorldStateInitResult.ErrWorldDimensionsZero:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World dimensions are zero");
                            return;
                        case WfcWorldStateInitResult.ErrRulesEmpty:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: No rules provided");
                            return;
                        case WfcWorldStateInitResult.ErrRulesHaveGaps:
                            // This is an error in our rules import, not a user error
                            Debug.Assert(false);
                            return;
                        default:
                            Debug.Assert(false);
                            return;
                    }
                }
            }

            unsafe {
                // Clear all modules to false first
                for (ushort z = 0; z < worldZ; ++z) {
                    for (ushort y = 0; y < worldY; ++y) {
                        for (ushort x = 0; x < worldX; ++x) {
                            for (ushort m = 0; m < nameToModule.Count; ++m) {
                                var result = Native.wfc_world_state_slot_module_set(wfcWorldStateHandle, x, y, z, m, 0);
                                switch (result) {
                                    case WfcWorldStateSlotModuleSetResult.Ok:
                                        break;
                                    case WfcWorldStateSlotModuleSetResult.ErrSlotOutOfBounds:
                                    case WfcWorldStateSlotModuleSetResult.ErrModuleOutOfBounds:
                                    default:
                                        Debug.Assert(false);
                                        return;
                                }
                            }
                        }
                    }
                }

                // Set modules provided by user
                for (var i = 0; i < worldSlotMinCount; ++i) {
                    var position = worldSlotPositions[i];
                    var moduleStr = worldSlotModules[i];

                    int xInt = (int)Math.Round(position.X);
                    int yInt = (int)Math.Round(position.Y);
                    int zInt = (int)Math.Round(position.Z);

                    if (xInt < 0 || xInt >= UInt16.MaxValue ||
                        yInt < 0 || yInt >= UInt16.MaxValue ||
                        zInt < 0 || zInt >= UInt16.MaxValue) {
                        AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                          "Slot positions must be nonnegative integers that fit in 16 bits");

                        Native.wfc_world_state_free(wfcWorldStateHandle);
                        return;
                    }

                    ushort x = (ushort)xInt;
                    ushort y = (ushort)yInt;
                    ushort z = (ushort)zInt;

                    if (nameToModule.TryGetValue(moduleStr.ToString(), out ushort m)) {
                        var result = Native.wfc_world_state_slot_module_set(wfcWorldStateHandle, x, y, z, m, 1);
                        switch (result) {
                            case WfcWorldStateSlotModuleSetResult.Ok:
                                break;
                            case WfcWorldStateSlotModuleSetResult.ErrSlotOutOfBounds:
                                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                                  "Provided slots must be within bounds of the world");

                                Native.wfc_world_state_free(wfcWorldStateHandle);
                                return;
                            case WfcWorldStateSlotModuleSetResult.ErrModuleOutOfBounds:
                            default:
                                Debug.Assert(false);
                                return;
                        }
                    } else {
                        AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                          "Slot module list (SM) contains module not found in the ruleset: " + moduleStr);

                        Native.wfc_world_state_free(wfcWorldStateHandle);
                        return;
                    }
                }

                {
                    var worldStatus = WorldStatus.Nondeterministic;
                    Native.wfc_world_state_canonicalize(wfcWorldStateHandle, &worldStatus);

                    switch (worldStatus) {
                        case WorldStatus.Deterministic:
                        case WorldStatus.Contradiction:
                            // A performance-minded implementation may choose to
                            // early out here for deterministic or contradictory
                            // world state. We don't do so for simplicity, and
                            // because calling wfc_observe with already
                            // deterministic or contradictory world status is
                            // not an error.

                            break;
                    }
                }

                Native.wfc_world_state_init_from(&wfcWorldStateHandleBackup, wfcWorldStateHandle);
                Native.wfc_rng_state_init(&wfcRngStateHandle, rngSeedLow, rngSeedHigh);
            }

            stats.ruleCount = (uint)adjacencyRulesMinCount;
            stats.moduleCount = (uint)moduleToName.Count;

            stats.inPhaseMillis = stopwatch.ElapsedMilliseconds;
            stopwatch.Stop();
            stopwatch.Reset();
            stopwatch.Start();

            uint attempts = 0;
            uint maxObservations = UInt32.MaxValue;
            uint spentObservations = 0;

            unsafe {
                while (true) {
                    var status = WorldStatus.Nondeterministic;
                    var result = Native.wfc_observe(wfcWorldStateHandle,
                                                    wfcRngStateHandle,
                                                    maxObservations,
                                                    &spentObservations,
                                                    &status);

                    attempts++;

                    if (result != WfcObserveResult.Ok) {
                        Debug.Assert(false);
                        return;
                    }

                    if (status == WorldStatus.Deterministic || (status == WorldStatus.Nondeterministic && attempts == maxAttempts)) {
                        break;
                    }

                    Native.wfc_world_state_clone_from(wfcWorldStateHandle, wfcWorldStateHandleBackup);
                }
            }

            stats.solveAttempts = attempts;

            stats.computePhaseMillis = stopwatch.ElapsedMilliseconds;
            stopwatch.Stop();
            stopwatch.Reset();
            stopwatch.Start();

            //
            // -- Output World state --
            //
            worldSlotPositions.Clear();
            worldSlotModules.Clear();

            unsafe {
                for (ushort z = 0; z < worldZ; ++z) {
                    for (ushort y = 0; y < worldY; ++y) {
                        for (ushort x = 0; x < worldX; ++x) {
                            for (ushort m = 0; m < nameToModule.Count; ++m) {
                                uint module_is_set = 0;
                                var result = Native.wfc_world_state_slot_module_get(wfcWorldStateHandle, x, y, z, m, &module_is_set);
                                switch (result) {
                                    case WfcWorldStateSlotModuleGetResult.Ok:
                                        break;
                                    case WfcWorldStateSlotModuleGetResult.ErrSlotOutOfBounds:
                                    case WfcWorldStateSlotModuleGetResult.ErrModuleOutOfBounds:
                                    default:
                                        Debug.Assert(false);
                                        return;
                                }

                                if (module_is_set == 1) {
                                    moduleToName.TryGetValue(m, out string moduleStr);

                                    worldSlotPositions.Add(new Vector3d(x, y, z));
                                    worldSlotModules.Add(moduleStr);
                                }
                            }
                        }
                    }
                }
            }

            Native.wfc_world_state_free(wfcWorldStateHandle);
            Native.wfc_world_state_free(wfcWorldStateHandleBackup);
            Native.wfc_rng_state_free(wfcRngStateHandle);

            stats.outPhaseMillis = stopwatch.ElapsedMilliseconds;
            stopwatch.Stop();

            DA.SetData(OUT_PARAM_DEBUG_OUTPUT, stats.ToString());
            DA.SetDataList(OUT_PARAM_WORLD_SLOT_POSITION, worldSlotPositions);
            DA.SetDataList(OUT_PARAM_WORLD_SLOT_MODULE, worldSlotModules);
        }
    }

    internal struct Stats {
        public uint ruleCount;
        public uint moduleCount;
        public uint solveAttempts;

        public long inPhaseMillis;
        public long computePhaseMillis;
        public long outPhaseMillis;

        public override string ToString() {
            StringBuilder b = new StringBuilder(256);

            b.Append("Rule count: ");
            b.Append(ruleCount);
            b.AppendLine();

            b.Append("Module count: ");
            b.Append(moduleCount);
            b.AppendLine();

            b.Append("Solve attempts: ");
            b.Append(solveAttempts);
            b.AppendLine();

            b.Append("In Phase time: ");
            b.Append(inPhaseMillis);
            b.Append("ms");
            b.AppendLine();

            b.Append("Compute Phase time: ");
            b.Append(computePhaseMillis);
            b.Append("ms");
            b.AppendLine();

            b.Append("Out Phase time: ");
            b.Append(outPhaseMillis);
            b.Append("ms");
            b.AppendLine();

            return b.ToString();
        }
    }

    internal enum AdjacencyRuleKind : uint {
        X = 0,
        Y = 1,
        Z = 2,
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct AdjacencyRule {
        public AdjacencyRuleKind kind;
        public ushort module_low;
        public ushort module_high;
    }

    internal enum WorldStatus : uint {
        Nondeterministic = 0,
        Deterministic = 1,
        Contradiction = 2,
    }

    internal enum WfcWorldStateInitResult : uint {
        Ok = 0,
        ErrModuleCountTooHigh = 1,
        ErrWorldDimensionsZero = 2,
        ErrRulesEmpty = 3,
        ErrRulesHaveGaps = 4,
    }

    internal enum WfcWorldStateCloneFromResult : uint {
        Ok = 0,
        ErrIncompatible = 1,
    }

    internal enum WfcWorldStateSlotModuleSetResult : uint {
        Ok = 0,
        ErrSlotOutOfBounds = 1,
        ErrModuleOutOfBounds = 2,
    }

    internal enum WfcWorldStateSlotModuleGetResult : uint {
        Ok = 0,
        ErrSlotOutOfBounds = 1,
        ErrModuleOutOfBounds = 2,
    }

    internal enum WfcWorldStateSlotModuleWeightSetResult : uint {
        Ok = 0,
        ErrSlotOutOfBounds = 1,
        ErrModuleOutOfBounds = 2,
        ErrWeightNotNormalPositive = 3,
    }

    internal enum WfcWorldStateCanonicalizeResult : uint {
        OkDeterministic = 0,
        OkNondeterministic = 1,
        OkContradiction = 2,
    }

    internal enum WfcObserveResult : uint {
        Ok = 0,
        ErrNotCanonical = 1,
    }

    internal class Native {

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint wfc_query_max_module_count();

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint wfc_feature_weighted_entropy();

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint wfc_feature_weighted_observation();

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateInitResult wfc_world_state_init(IntPtr* wfc_world_state_handle_ptr,
                                                                                   AdjacencyRule* adjacency_rules_ptr,
                                                                                   UIntPtr adjacency_rules_len,
                                                                                   ushort world_x,
                                                                                   ushort world_y,
                                                                                   ushort world_z,
                                                                                   uint features);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_init_from(IntPtr* wfc_world_state_handle_ptr,
                                                                     IntPtr source_wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateCloneFromResult wfc_world_state_clone_from(IntPtr destination_wfc_world_state_handle,
                                                                                              IntPtr source_wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern void wfc_world_state_free(IntPtr wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern WfcWorldStateSlotModuleSetResult wfc_world_state_slot_module_set(IntPtr wfc_world_state_handle,
                                                                                                ushort pos_x,
                                                                                                ushort pos_y,
                                                                                                ushort pos_z,
                                                                                                ushort module,
                                                                                                uint module_is_set);
        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateSlotModuleGetResult wfc_world_state_slot_module_get(IntPtr wfc_world_state_handle,
                                                                                                       ushort pos_x,
                                                                                                       ushort pos_y,
                                                                                                       ushort pos_z,
                                                                                                       ushort module,
                                                                                                       uint* module_is_set);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateSlotModuleWeightSetResult wfc_world_state_slot_module_weight_set(IntPtr wfc_world_state_handle,
                                                                                                                    ushort pos_x,
                                                                                                                    ushort pos_y,
                                                                                                                    ushort pos_z,
                                                                                                                    ushort module,
                                                                                                                    float weight);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_rng_state_init(IntPtr* wfc_rng_state_handle_ptr,
                                                              ulong rng_seed_low,
                                                              ulong rng_seed_high);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_rng_state_free(IntPtr wfc_rng_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_canonicalize(IntPtr wfc_world_state_handle,
                                                                        WorldStatus* world_status);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcObserveResult wfc_observe(IntPtr wfc_world_state_handle,
                                                                   IntPtr wfc_rng_state_handle,
                                                                   uint max_observations,
                                                                   uint* spent_observations,
                                                                   WorldStatus* world_status);
    }
}
