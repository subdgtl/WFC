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
        private const int IN_PARAM_RANDOM_SEED = 6;
        private const int IN_PARAM_MAX_ATTEMPTS = 7;

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

            List<string> adjacencyRulesAxis = new List<string>();
            List<string> adjacencyRulesModuleLow = new List<string>();
            List<string> adjacencyRulesModuleHigh = new List<string>();
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

            // We need to check ahead of time, if there are at most 256 modules
            // altogether in the input, otherwise the `nextModule` variable will
            // overflow and cause a dictionary error.
            {
                HashSet<string> allModules = new HashSet<string>();

                for (int i = 0; i < adjacencyRulesMinCount; ++i)
                {
                    allModules.Add(adjacencyRulesModuleLow[i]);
                    allModules.Add(adjacencyRulesModuleHigh[i]);
                }

                if (allModules.Count > 256)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "Too many modules. Maximum allowed is 256");
                    return;
                }
            }

            byte nextModule = 0;
            Dictionary<string, byte> nameToModule = new Dictionary<string, byte>();
            Dictionary<byte, string> moduleToName = new Dictionary<byte, string>();
            AdjacencyRule[] adjacencyRules = new AdjacencyRule[adjacencyRulesMinCount];

            for (int i = 0; i < adjacencyRulesMinCount; ++i)
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

            Vector3d worldSize = Vector3d.Zero;
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

            // This array is re-used for both input and output (if input world state was provided).
            // This is ok, because wfc_world_state_get does clear it to zero before writing to it.
            SlotState[] worldState = new SlotState[worldDimensions];

            // ... WE do need to clear it to zero, however. C# does not initialize slot_state for us!
            for (int i = 0; i < worldState.Length; ++i)
            {
                unsafe
                {
                    worldState[i].slot_state[0] = 0;
                    worldState[i].slot_state[1] = 0;
                    worldState[i].slot_state[2] = 0;
                    worldState[i].slot_state[3] = 0;
                }
            }

            // Note: These lists will be cleared and re-used for output.
            List<Vector3d> worldSlotPositions = new List<Vector3d>();
            List<string> worldSlotModules = new List<string>();

            DA.GetDataList(IN_PARAM_WORLD_SLOT_POSITION, worldSlotPositions);
            DA.GetDataList(IN_PARAM_WORLD_SLOT_MODULE, worldSlotModules);

            int worldSlotMinCount = Math.Min(worldSlotPositions.Count, worldSlotModules.Count);
            for (int i = 0; i < worldSlotMinCount; ++i)
            {
                Vector3d position = worldSlotPositions[i];
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
                    Debug.Assert(slotIndex < worldState.Length);

                    byte blkIndex = (byte)(module / 64u);
                    byte bitIndex = (byte)(module % 64u);
                    ulong mask = 1ul << bitIndex;

                    Debug.Assert(blkIndex < 4);
                    unsafe
                    {
                        worldState[slotIndex].slot_state[blkIndex] |= mask;
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
            // -- Random seed --
            //
            // wfc_init needs 128 bits worth of random seed, but that is tricky to provide from GH.
            // We let GH provide an int, use it to seed a C# Random, get 16 bytes of data from that
            // and copy those into two u64's.

            int randomSeed = 0;
            DA.GetData(IN_PARAM_RANDOM_SEED, ref randomSeed);

            Random random = new Random(randomSeed);
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

            Int32 maxAttemptsInt = 0;
            DA.GetData(IN_PARAM_MAX_ATTEMPTS, ref maxAttemptsInt);
            uint maxAttempts = (uint)maxAttemptsInt;

            //
            // -- Run the thing and **pray** --
            //

            IntPtr wfc = IntPtr.Zero;
            unsafe
            {
                fixed (AdjacencyRule* adjacencyRulesPtr = &adjacencyRules[0])
                {
                    WfcInitResult result = Native.wfc_init(&wfc,
                                                           adjacencyRulesPtr,
                                                           (UIntPtr)adjacencyRules.Length,
                                                           worldX,
                                                           worldY,
                                                           worldZ,
                                                           rngSeedLow,
                                                           rngSeedHigh);

                    switch (result)
                    {
                        case WfcInitResult.Ok:
                            // All good
                            break;
                        case WfcInitResult.TooManyModules:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: Adjacency rules contained too many modules");
                            return;
                        case WfcInitResult.WorldDimensionsZero:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World dimensions are zero");
                            return;
                        default:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed with unknown error");
                            return;
                    }
                }

                fixed (SlotState* worldStatePtr = &worldState[0])
                {
                    WfcWorldStateSetResult result = Native.wfc_world_state_set(wfc,
                                                                               worldStatePtr,
                                                                               (UIntPtr)worldState.Length);
                    switch (result)
                    {
                        case WfcWorldStateSetResult.Ok:
                            // All good
                            stats.worldNotCanonical = false;
                            break;
                        case WfcWorldStateSetResult.OkNotCanonical:
                            // All good, but we had to fix some things
                            stats.worldNotCanonical = true;
                            break;
                        case WfcWorldStateSetResult.WorldContradictory:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World state is contradictory");
                            return;
                    }
                }
            }

            uint attempts = Native.wfc_observe(wfc, maxAttempts);
            if (attempts == 0)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "WFC solver failed to find solution within " + maxAttempts + " attempts");
                return;
            }

            unsafe
            {
                fixed (SlotState* worldStatePtr = &worldState[0])
                {
                    Native.wfc_world_state_get(wfc, worldStatePtr, (UIntPtr)worldState.Length);
                }
            }

            Native.wfc_free(wfc);

            //
            // -- Output: World state --
            //
            // The resulting world state is in the flat bit-vector format. Since we
            // early-out on nondeterministic results, we can assume exactly one bit
            // being set here and can therefore produce a flat list on output.

            worldSlotPositions.Clear();
            worldSlotModules.Clear();
            for (int i = 0; i < worldState.Length; ++i)
            {
                // Assume the result is deterministic and only take the first set bit
                short module = short.MinValue;
                for (int blkIndex = 0; blkIndex < 4 && module == short.MinValue; ++blkIndex)
                {
                    for (int bitIndex = 0; bitIndex < 64 && module == short.MinValue; ++bitIndex)
                    {
                        ulong mask = 1ul << bitIndex;
                        unsafe
                        {
                            if ((worldState[i].slot_state[blkIndex] & mask) != 0)
                            {
                                module = (short)(64 * blkIndex + bitIndex);
                            }
                        }
                    }
                }

                string moduleStr = "<unknown>";
                if (module >= 0)
                {
                    Debug.Assert(module <= byte.MaxValue);
                    moduleToName.TryGetValue((byte)module, out moduleStr);
                }

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

    internal enum WfcInitResult : uint
    {
        Ok = 0,
        TooManyModules = 1,
        WorldDimensionsZero = 2,
    }

    internal enum WfcWorldStateSetResult : uint
    {
        Ok = 0,
        OkNotCanonical = 1,
        WorldContradictory = 2,
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
        internal static unsafe extern WfcInitResult wfc_init(IntPtr* wfc_ptr,
                                                             AdjacencyRule* adjacency_rules_ptr,
                                                             UIntPtr adjacency_rules_len,
                                                             ushort world_x,
                                                             ushort world_y,
                                                             ushort world_z,
                                                             ulong rngSeedLow,
                                                             ulong rngSeedHigh);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern void wfc_free(IntPtr wfc);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static extern uint wfc_observe(IntPtr wfc, uint max_attempts);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern WfcWorldStateSetResult wfc_world_state_set(IntPtr wfc,
                                                                                 SlotState* world_state_ptr,
                                                                                 UIntPtr world_state_len);

        [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
        internal static unsafe extern void wfc_world_state_get(IntPtr wfc,
                                                               SlotState* world_state_ptr,
                                                               UIntPtr world_state_len);
    }

}