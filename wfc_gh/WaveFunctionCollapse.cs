using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using Grasshopper.Kernel;
using Rhino.Geometry;

namespace wfc_gh
{
    public class GH_WaveFunctionCollapse3D : GH_Component
    {

        private const int IN_PARAM_RULE_AXIS = 0;
        private const int IN_PARAM_RULE_LOW = 1;
        private const int IN_PARAM_RULE_HIGH = 2;
        private const int IN_PARAM_WORLD_SIZE = 3;
        private const int IN_PARAM_WORLD_SLOT_POSITION = 4;
        private const int IN_PARAM_WORLD_SLOT_MODULE = 5;
        private const int IN_PARAM_ENTROPY = 6;
        private const int IN_PARAM_RANDOM_SEED = 7;
        private const int IN_PARAM_MAX_ATTEMPTS = 8;

        private const int OUT_PARAM_DEBUG_OUTPUT = 0;
        private const int OUT_PARAM_WORLD_SLOT_POSITION = 1;
        private const int OUT_PARAM_WORLD_SLOT_MODULE = 2;

        private System.Drawing.Bitmap componentIcon;

        public GH_WaveFunctionCollapse3D() : base("Wave Function Collapse 3D",
                                                  "WFC3",
                                                  "Solver for the Wave Function Collapse algorithm in cubic voxel space",
                                                  "WaveFunctionCollapse",
                                                  "WaveFunctionCollapse")
        {
            using (var ms = new MemoryStream(Resources.ComponentIcon))
            {
                componentIcon = new System.Drawing.Bitmap(ms);
            }
        }

        public override Guid ComponentGuid => new Guid("05d6620c-8bb7-44f0-8375-a8120ee0c9b2");

        protected override System.Drawing.Bitmap Icon => componentIcon;

        protected override void RegisterInputParams(GH_InputParamManager pManager)
        {
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

            pManager.AddBooleanParameter("Use Shannon Entropy",
                                         "E",
                                         "Whether to use Shannon Entropy instead of the simpler linear entropy calculations",
                                         GH_ParamAccess.item,
                                         false);

            pManager.AddIntegerParameter("Random seed",
                                         "S",
                                         "Random number genenerator seed",
                                         GH_ParamAccess.item,
                                         42);
            pManager.AddIntegerParameter("Max attempts",
                                         "A",
                                         "Maximum number of tries before the solver gives up",
                                         GH_ParamAccess.item,
                                         100);
        }

        protected override void RegisterOutputParams(GH_OutputParamManager pManager)
        {
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

        protected override void SolveInstance(IGH_DataAccess DA)
        {
            Stats stats = new Stats();

            //
            // -- Adjacency rules --
            //
            // The rules come in three lists of the same length. The first contains texts
            // representing the axis/kind (x/y/z).
            // Second and third list contain unique textual identifiers of the modules.
            // This importer replaces those string names with generated u32 numbers,
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

            if (adjacencyRulesMinCount == 0)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "Must supply at least one adjacency rule");
                return;
            }

            // Check ahead of time, if there are at most maxModuleCount modules
            // altogether in the input.
            uint maxModuleCount = Native.wfc_max_module_count_get();
            {
                HashSet<string> allModules = new HashSet<string>();

                for (var i = 0; i < adjacencyRulesMinCount; ++i)
                {
                    allModules.Add(adjacencyRulesModuleLow[i]);
                    allModules.Add(adjacencyRulesModuleHigh[i]);
                }

                if (allModules.Count > maxModuleCount)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Too many modules. Maximum allowed is " + maxModuleCount);
                    return;
                }
            }

            byte nextModule = 0;
            var nameToModule = new Dictionary<string, byte>();
            var moduleToName = new Dictionary<byte, string>();
            var adjacencyRules = new AdjacencyRule[adjacencyRulesMinCount];

            for (var i = 0; i < adjacencyRulesMinCount; ++i)
            {
                string axisStr = adjacencyRulesAxis[i];
                string lowStr = adjacencyRulesModuleLow[i];
                string highStr = adjacencyRulesModuleHigh[i];

                AdjacencyRuleKind kind;
                switch (axisStr)
                {
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

                byte low = 0;
                if (nameToModule.ContainsKey(lowStr))
                {
                    nameToModule.TryGetValue(lowStr, out low);
                }
                else
                {
                    low = nextModule;
                    nameToModule.Add(lowStr, low);
                    moduleToName.Add(low, lowStr);
                    nextModule++;
                    Debug.Assert(nextModule < maxModuleCount);
                }

                byte high = 0;
                if (nameToModule.ContainsKey(highStr))
                {
                    nameToModule.TryGetValue(highStr, out high);
                }
                else
                {
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

            if (worldXInt <= 0)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World X must be a positive integer");
                return;
            }
            if (worldYInt <= 0)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World Y must be a positive integer");
                return;
            }
            if (worldZInt <= 0)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World Z must be a positive integer");
                return;
            }

            ushort worldX = (ushort)worldXInt;
            ushort worldY = (ushort)worldYInt;
            ushort worldZ = (ushort)worldZInt;
            uint worldDimensions = (uint)worldX * worldY * worldZ;
            uint worldSlotsPerLayer = (uint)worldX * worldY;
            uint worldSlotsPerRow = worldX;

            //
            // -- World slot positions and modules --
            //

            // This array is re-used for both input and output (if input world
            // state was provided). This is ok, because
            // wfc_world_state_slots_get does clear it to zero before writing to
            // it.
            var slots = new SlotState[worldDimensions];

            // ... WE do need to clear it to zero, however. C# does not
            // initialize slot_state for us!
            for (var i = 0; i < slots.Length; ++i)
            {
                unsafe
                {
                    slots[i].slot_state[0] = 0;
                    slots[i].slot_state[1] = 0;
                    slots[i].slot_state[2] = 0;
                    slots[i].slot_state[3] = 0;
                }
            }

            // Note: These lists will be cleared and re-used for output.
            var worldSlotPositions = new List<Vector3d>();
            var worldSlotModules = new List<string>();

            DA.GetDataList(IN_PARAM_WORLD_SLOT_POSITION, worldSlotPositions);
            DA.GetDataList(IN_PARAM_WORLD_SLOT_MODULE, worldSlotModules);

            int worldSlotMinCount = Math.Min(worldSlotPositions.Count, worldSlotModules.Count);
            for (var i = 0; i < worldSlotMinCount; ++i)
            {
                var position = worldSlotPositions[i];
                string moduleStr = worldSlotModules[i];

                int slotXInt = (int)Math.Round(position.X);
                int slotYInt = (int)Math.Round(position.Y);
                int slotZInt = (int)Math.Round(position.Z);

                if (slotXInt < 0)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Slot X must be a nonnegative integer");
                    return;
                }
                if (slotYInt < 0)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Slot Y must be a nonnegative integer");
                    return;
                }
                if (slotZInt < 0)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "World Z must be a nonnegative integer");
                    return;
                }

                uint slotX = (uint)slotXInt;
                uint slotY = (uint)slotYInt;
                uint slotZ = (uint)slotZInt;

                if (nameToModule.TryGetValue(moduleStr.ToString(), out byte module))
                {
                    uint slotIndex = slotX + slotY * worldSlotsPerRow + slotZ * worldSlotsPerLayer;
                    Debug.Assert(slotIndex < slots.Length);

                    byte blkIndex = (byte)(module / 64u);
                    byte bitIndex = (byte)(module % 64u);

                    Debug.Assert(blkIndex < 4);
                    unsafe
                    {
                        slots[slotIndex].slot_state[blkIndex] |= 1ul << bitIndex;
                    }
                }
                else
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Slot module list (SM) contains module not found in the ruleset: " + moduleStr);
                    return;
                }
            }

            //
            // -- Entropy --
            //

            bool useShannonEntropy = false;
            DA.GetData(IN_PARAM_ENTROPY, ref useShannonEntropy);

            Entropy entropy = Entropy.Linear;
            if (useShannonEntropy) {
                entropy = Entropy.Shannon;
            }

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

            if (!BitConverter.IsLittleEndian)
            {
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
            // -- Run the thing and **pray** --
            //

            var wfcRngStateHandle = IntPtr.Zero;
            var wfcWorldStateHandle = IntPtr.Zero;
            var wfcWorldStateHandleBackup = IntPtr.Zero;
            unsafe
            {
                Native.wfc_rng_state_init(&wfcRngStateHandle, rngSeedLow, rngSeedHigh);

                fixed (AdjacencyRule* adjacencyRulesPtr = &adjacencyRules[0])
                {
                    var result = Native.wfc_world_state_init(&wfcWorldStateHandle,
                                                             adjacencyRulesPtr,
                                                             (UIntPtr)adjacencyRules.Length,
                                                             worldX,
                                                             worldY,
                                                             worldZ,
                                                             entropy);

                    switch (result)
                    {
                        case WfcWorldStateInitResult.Ok:
                            // All good
                            break;
                        case WfcWorldStateInitResult.ErrTooManyModules:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: Adjacency rules contained too many modules");
                            return;
                        case WfcWorldStateInitResult.ErrWorldDimensionsZero:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World dimensions are zero");
                            return;
                        default:
                            Debug.Assert(false);
                            return;
                    }
                }

                fixed (SlotState* slotsPtr = &slots[0])
                {
                    var result = Native.wfc_world_state_slots_set(wfcWorldStateHandle,
                                                                  slotsPtr,
                                                                  (UIntPtr)slots.Length);
                    switch (result)
                    {
                        case WfcWorldStateSlotsSetResult.Ok:
                            // All good
                            stats.worldNotCanonical = false;
                            break;
                        case WfcWorldStateSlotsSetResult.OkWorldNotCanonical:
                            // All good, but we the slots we gave were not
                            // canonical. wfc_world_state_slots_set fixed that for us.
                            stats.worldNotCanonical = true;
                            break;
                        case WfcWorldStateSlotsSetResult.ErrWorldContradictory:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World state is contradictory");
                            return;
                        default:
                            Debug.Assert(false);
                            return;
                    }
                }

                Native.wfc_world_state_init_from(&wfcWorldStateHandleBackup, wfcWorldStateHandle);
            }

            bool foundDeterministic = false;
            uint attempts = 0;

            uint maxObservations = UInt32.MaxValue;

            var spentObservations = IntPtr.Zero;

            if (maxObservations == 0){
                var result = Native.wfc_world_status(wfcWorldStateHandle);
                if (result == WfcObserveResult.Deterministic)
                        {
                            foundDeterministic = true;
                        }
            } else {
                unsafe{
                    while (!foundDeterministic && attempts < maxAttempts)
                    {
                        var result = Native.wfc_observe(wfcWorldStateHandle,
                                                        wfcRngStateHandle,
                                                        (UIntPtr)maxObservations,
                                                        &spentObservations);
                        if (result == WfcObserveResult.Deterministic)
                        {
                            foundDeterministic = true;
                        }
                        else
                        {
                            Native.wfc_world_state_clone_from(wfcWorldStateHandle,
                                                            wfcWorldStateHandleBackup);
                        }

                        attempts++;
                    }
                }
            }


            if (!foundDeterministic)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "WFC solver failed to find solution within " + maxAttempts + " attempts");
                return;
            }

            unsafe
            {
                fixed (SlotState* slotsPtr = &slots[0])
                {
                    Native.wfc_world_state_slots_get(wfcWorldStateHandle,
                                                     slotsPtr,
                                                     (UIntPtr)slots.Length);
                }
            }

            Native.wfc_world_state_free(wfcWorldStateHandle);
            Native.wfc_rng_state_free(wfcRngStateHandle);

            //
            // -- Output: World state --
            //
            // The resulting world state is in the flat bit-vector format. Since we
            // early-out on nondeterministic results, we can assume exactly one bit
            // being set here and can therefore produce a flat list on output.

            worldSlotPositions.Clear();
            worldSlotModules.Clear();
            for (var i = 0; i < slots.Length; ++i)
            {
                // Because WFC finished successfully, we assume the result is
                // deterministic and only take the first set bit. short.MinValue
                // is used as a sentinel for uninitialized and we later assert
                // it has been set.
                short module = short.MinValue;
                for (int blkIndex = 0; blkIndex < 4 && module == short.MinValue; ++blkIndex)
                {
                    for (int bitIndex = 0; bitIndex < 64 && module == short.MinValue; ++bitIndex)
                    {
                        unsafe
                        {
                            if ((slots[i].slot_state[blkIndex] & (1ul << bitIndex)) != 0)
                            {
                                module = (short)(64 * blkIndex + bitIndex);
                            }
                        }
                    }
                }

                string moduleStr = "<unknown>";
                Debug.Assert(module >= 0);
                Debug.Assert(module <= byte.MaxValue);
                Debug.Assert(module < maxModuleCount);
                moduleToName.TryGetValue((byte)module, out moduleStr);

                long slotX = i % worldSlotsPerLayer % worldSlotsPerRow;
                long slotY = i % worldSlotsPerLayer / worldSlotsPerRow;
                long slotZ = i / worldSlotsPerLayer;

                worldSlotPositions.Add(new Vector3d(slotX, slotY, slotZ));
                worldSlotModules.Add(moduleStr);
            }

            stats.ruleCount = (uint)adjacencyRulesMinCount;
            stats.moduleCount = (uint)moduleToName.Count;
            stats.solveAttempts = attempts;

            DA.SetData(OUT_PARAM_DEBUG_OUTPUT, stats.ToString());
            DA.SetDataList(OUT_PARAM_WORLD_SLOT_POSITION, worldSlotPositions);
            DA.SetDataList(OUT_PARAM_WORLD_SLOT_MODULE, worldSlotModules);
        }
    }

    internal enum AdjacencyRuleKind : uint
    {
        X = 0,
        Y = 1,
        Z = 2,
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct AdjacencyRule
    {
        public AdjacencyRuleKind kind;
        public byte module_low;
        public byte module_high;
    }

    internal enum Entropy : uint
    {
        Linear = 0,
        Shannon = 1,
    }

    internal enum WfcWorldStateInitResult : uint
    {
        Ok = 0,
        ErrTooManyModules = 1,
        ErrWorldDimensionsZero = 2,
    }

    internal enum WfcWorldStateSlotsSetResult : uint
    {
        Ok = 0,
        OkWorldNotCanonical = 1,
        ErrWorldContradictory = 2,
    }

    internal enum WfcObserveResult: uint {
        Deterministic = 0,
        Contradiction = 1,
    }

    [StructLayout(LayoutKind.Sequential)]
    internal unsafe struct SlotState
    {
        public fixed ulong slot_state[4];

        public override string ToString()
        {
            StringBuilder b = new StringBuilder("Slot state { ", 64);

            b.Append("[");
            b.Append(slot_state[0]);
            b.Append("][");
            b.Append(slot_state[1]);
            b.Append("][");
            b.Append(slot_state[2]);
            b.Append("][");
            b.Append(slot_state[3]);
            b.Append("] }");

            return b.ToString();
        }
    }

    internal struct Stats
    {
        public uint ruleCount;
        public uint moduleCount;
        public uint solveAttempts;
        public bool worldNotCanonical;

        public override string ToString()
        {
            StringBuilder b = new StringBuilder(128);

            b.Append("Rule count: ");
            b.Append(ruleCount);
            b.AppendLine();
            b.Append("Module count: ");
            b.Append(moduleCount);
            b.AppendLine();
            b.Append("Solve attempts: ");
            b.Append(solveAttempts);
            b.AppendLine();

            if (worldNotCanonical)
            {
                b.AppendLine("Warning: Initial world state is not canonical");
            }

            return b.ToString();
        }
    }

    internal class Native
    {
        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint wfc_max_module_count_get();

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateInitResult wfc_world_state_init(IntPtr* wfc_world_state_handle_ptr,
                                                                                   AdjacencyRule* adjacency_rules_ptr,
                                                                                   UIntPtr adjacency_rules_len,
                                                                                   ushort world_x,
                                                                                   ushort world_y,
                                                                                   ushort world_z,
                                                                                   Entropy entropy);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_init_from(IntPtr* wfc_world_state_handle_ptr,
                                                                     IntPtr source_wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_clone_from(IntPtr destination_wfc_world_state_handle,
                                                                      IntPtr source_wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern void wfc_world_state_free(IntPtr wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateSlotsSetResult wfc_world_state_slots_set(IntPtr wfc_world_state_handle,
                                                                                            SlotState* slots_ptr,
                                                                                            UIntPtr slots_len);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_slots_get(IntPtr wfc_world_state_handle,
                                                                     SlotState* slots_ptr,
                                                                     UIntPtr slots_len);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern WfcObserveResult wfc_world_status(IntPtr wfc_world_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_rng_state_init(IntPtr* wfc_rng_state_handle_ptr,
                                                              ulong rng_seed_low,
                                                              ulong rng_seed_high);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern void wfc_rng_state_free(IntPtr wfc_rng_state_handle);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcObserveResult wfc_observe(IntPtr wfc_world_state_handle,
                                                            IntPtr wfc_rng_state_handle,
                                                            UIntPtr max_observations,
                                                            IntPtr* observationsSpent);
    }

}
