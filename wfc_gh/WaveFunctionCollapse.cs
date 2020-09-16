using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;
using Grasshopper;
using Grasshopper.Kernel;
using Grasshopper.Kernel.Data;
using Grasshopper.Kernel.Types;

public class GH_WaveFunctionCollapse3D : GH_Component
{

    private const int IN_PARAM_RULE_AXIS = 0;
    private const int IN_PARAM_RULE_LOW = 1;
    private const int IN_PARAM_RULE_HIGH = 2;
    private const int IN_PARAM_WORLD_X = 3;
    private const int IN_PARAM_WORLD_Y = 4;
    private const int IN_PARAM_WORLD_Z = 5;
    private const int IN_PARAM_WORLD_STATE = 6;
    private const int IN_PARAM_RANDOM_SEED = 7;
    private const int IN_PARAM_MAX_ATTEMPTS = 8;

    private const int OUT_PARAM_DEBUG_OUTPUT = 0;
    private const int OUT_PARAM_WORLD_STATE = 1;
    public GH_WaveFunctionCollapse3D() : base("Wave Function Collapse 3D",
                                              "WFC3",
                                              "Solver for the Wave Function Collapse algorithm in cubic voxel space",
                                              "WaveFunctionCollapse",
                                              "WaveFunctionCollapse")
    {
    }

    public override Guid ComponentGuid => new Guid("05d6620c-8bb7-44f0-8375-a8120ee0c9b2");

    protected override void RegisterInputParams(GH_InputParamManager pManager)
    {
        pManager.AddTextParameter("Rule axis",
                                  "RA",
                                  "Axis along which to apply the rule",
                                  GH_ParamAccess.list);
        pManager.AddTextParameter("Rule low",
                                  "RL",
                                  "Identifier of the module on the low dimension along the axis",
                                  GH_ParamAccess.list);
        pManager.AddTextParameter("Rule high",
                                  "RH",
                                  "Identifier of the module on the high dimension along the axis",
                                  GH_ParamAccess.list);

        pManager.AddIntegerParameter("World X",
                                     "X",
                                     "X dimension of the world",
                                     GH_ParamAccess.item,
                                     5);
        pManager.AddIntegerParameter("World Y",
                                     "Y",
                                     "Y dimension of the world",
                                     GH_ParamAccess.item,
                                     5);
        pManager.AddIntegerParameter("World Z",
                                     "Z",
                                     "Z dimension of the world",
                                     GH_ParamAccess.item,
                                     5);

        pManager.AddTextParameter("World state",
                                  "W",
                                  "The initial state of the world. Pass in a tree with exactly one path as a shorthand to allow every module in every slot",
                                  GH_ParamAccess.tree);

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
        pManager.AddTextParameter("World state",
                                  "W",
                                  "State of the world once the wave function has collapsed",
                                  GH_ParamAccess.tree);
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

        int minCount = Math.Min(Math.Min(adjacencyRulesAxis.Count, adjacencyRulesModuleLow.Count),
                                adjacencyRulesModuleHigh.Count);

        if (minCount == 0)
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

            for (int i = 0; i < minCount; ++i) {
                allModules.Add(adjacencyRulesModuleLow[i]);
                allModules.Add(adjacencyRulesModuleHigh[i]);
            }

            if (allModules.Count > 256) {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "Too many modules. Maximum allowed is 256");
                return;
            }
        }

        byte nextModule = 0;
        Dictionary<string, byte> nameToModule = new Dictionary<string, byte>();
        Dictionary<byte, string> moduleToName = new Dictionary<byte, string>();
        AdjacencyRule[] adjacencyRules = new AdjacencyRule[minCount];

        for (int i = 0; i < minCount; ++i)
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

        int worldXInt = 0;
        int worldYInt = 0;
        int worldZInt = 0;
        DA.GetData(IN_PARAM_WORLD_X, ref worldXInt);
        DA.GetData(IN_PARAM_WORLD_Y, ref worldYInt);
        DA.GetData(IN_PARAM_WORLD_Z, ref worldZInt);

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
        uint worldDimensions = (uint)worldX * (uint)worldY * (uint)worldZ;

        //
        // -- World state --
        //

        // This array is re-used for both input and output (if input world state was provided).
        // This is ok, because wfc_world_state_get does clear it to zero.
        SlotState[] worldState = new SlotState[worldDimensions];

        GH_Structure<GH_String> worldStateTree;
        DA.GetDataTree<GH_String>(IN_PARAM_WORLD_STATE, out worldStateTree);

        // Everything in GH is a tree and has at least one branch. GH doesn't seem to support
        // default values for tree parameters - it doesn't even call SolveInstance unless there
        // is a value plugged in - so we let the user use the default parameter by plugging in
        // any tree with 1 branch or less.
        bool worldStateProvided = worldStateTree.PathCount > 1;
        if (worldStateProvided)
        {
            if (worldStateTree.PathCount != worldDimensions)
            {
                AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                  "World state must have the same amount of branches as 'WorldX * WorldY * WorldZ'");
                return;
            }

            foreach (GH_Path path in worldStateTree.Paths)
            {
                if (!path.Valid)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "World state data tree must have branches with valid paths");
                    return;
                }

                if (path.Length != 1)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "World state data tree must have branches with paths of length one");
                    return;
                }

                Int32 branchIndex = path.Indices[0];
                if (branchIndex < 0 || branchIndex >= worldDimensions)
                {
                    AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                      "World state data tree must have branches with paths of addressing between 0 and 'WorldX * WorldY * WorldZ'");
                    return;
                }

                SlotState slotState;
                // C# does not initialize slot_state for us...
                unsafe
                {
                    slotState.slot_state[0] = 0;
                    slotState.slot_state[1] = 0;
                    slotState.slot_state[2] = 0;
                    slotState.slot_state[3] = 0;
                }

                IList branchValues = worldStateTree.get_Branch(branchIndex);
                foreach (GH_String name in branchValues)
                {
                    if (nameToModule.TryGetValue(name.ToString(), out byte module))
                    {
                        byte blkIdx = (byte)(module / 64u);
                        byte bitIdx = (byte)(module % 64u);
                        ulong mask = 1ul << bitIdx;

                        Debug.Assert(blkIdx < 4);
                        unsafe
                        {
                            slotState.slot_state[blkIdx] |= mask;
                        }
                    } else {
                        AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                          "World state data contains module not found in the ruleset: " + name);
                        return;
                    }
                }

                worldState[branchIndex] = slotState;
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

            if (worldStateProvided)
            {
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

        DataTree<string> worldStateOutput = new DataTree<string>();

        {
            Int32 branchIndex = 0;
            foreach (SlotState slotState in worldState)
            {
                // Assume the result is deterministic and only take the first set bit
                short module = short.MinValue;
                for (int blkIdx = 0; blkIdx < 4 && module == short.MinValue; ++blkIdx)
                {
                    for (int bitIdx = 0; bitIdx < 64 && module == short.MinValue; ++bitIdx)
                    {
                        ulong mask = 1ul << bitIdx;
                        unsafe
                        {
                            if ((slotState.slot_state[blkIdx] & mask) != 0)
                            {
                                module = (short)(64 * blkIdx + bitIdx);
                            }
                        }
                    }
                }

                string name = "<unknown>";
                if (module >= 0)
                {
                    Debug.Assert(module <= byte.MaxValue);
                    moduleToName.TryGetValue((byte)module, out name);
                }

                GH_Path branchPath = new GH_Path(branchIndex);
                worldStateOutput.Add(name, branchPath);

                branchIndex++;
            }
        }

        stats.ruleCount = (uint)minCount;
        stats.moduleCount = (uint)moduleToName.Count;
        stats.solveAttempts = attempts;

        DA.SetData(OUT_PARAM_DEBUG_OUTPUT, stats.ToString());
        DA.SetDataTree(OUT_PARAM_WORLD_STATE, worldStateOutput);
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
