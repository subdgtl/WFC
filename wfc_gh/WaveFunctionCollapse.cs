using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using Grasshopper;
using Grasshopper.Kernel;
using Grasshopper.Kernel.Data;
using Grasshopper.Kernel.Types;

public class GH_WaveFunctionCollapse3D : GH_Component
{

    private const Int32 IN_PARAM_RULE_AXIS = 0;
    private const Int32 IN_PARAM_RULE_LOW = 1;
    private const Int32 IN_PARAM_RULE_HIGH = 2;
    private const Int32 IN_PARAM_WORLD_X = 3;
    private const Int32 IN_PARAM_WORLD_Y = 4;
    private const Int32 IN_PARAM_WORLD_Z = 5;
    private const Int32 IN_PARAM_WORLD_STATE = 6;
    private const Int32 IN_PARAM_RANDOM_SEED = 7;
    private const Int32 IN_PARAM_MAX_ATTEMPTS = 8;

    private const Int32 OUT_PARAM_DEBUG_OUTPUT = 0;
    private const Int32 OUT_PARAM_WORLD_STATE = 1;
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
                                  "The initial state of the world",
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

        Int32 minCount = Math.Min(Math.Min(adjacencyRulesAxis.Count, adjacencyRulesModuleLow.Count),
                                  adjacencyRulesModuleHigh.Count);

        if (minCount == 0)
        {
            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                              "Must supply at least one adjacency rule");
            return;
        }

        UInt32 nextModule = 0;
        Dictionary<string, UInt32> nameToModule = new Dictionary<string, UInt32>();
        Dictionary<UInt32, string> moduleToName = new Dictionary<UInt32, string>();
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

            UInt32 low = 0;
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

            UInt32 high = 0;
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

        Int32 worldXInt = 0;
        Int32 worldYInt = 0;
        Int32 worldZInt = 0;
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

        UInt16 worldX = (UInt16)worldXInt;
        UInt16 worldY = (UInt16)worldYInt;
        UInt16 worldZ = (UInt16)worldZInt;
        UInt32 worldDimensions = (UInt32)worldX * (UInt32)worldY * (UInt32)worldZ;

        //
        // -- World state --
        //

        // This array is re-used for both input and output (if input world state was provided).
        // This is ok, because wfc_world_state_get does clear it to zero.
        SlotState[] worldState = new SlotState[worldDimensions];

        GH_Structure<GH_String> worldStateTree;
        bool worldStateProvided = DA.GetDataTree<GH_String>(IN_PARAM_WORLD_STATE, out worldStateTree);
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
                // Oh, God C#. WHY?! WHERE ARE THE SANE DEFAULTS. WHAT DO YOU SEE?
                unsafe
                {
                    slotState.slot_state[0] = 0;
                    slotState.slot_state[1] = 0;
                    slotState.slot_state[2] = 0;
                    slotState.slot_state[3] = 0;
                    slotState.slot_state[4] = 0;
                    slotState.slot_state[5] = 0;
                    slotState.slot_state[6] = 0;
                    slotState.slot_state[7] = 0;
                }

                IList branchValues = worldStateTree.get_Branch(branchIndex);
                foreach (GH_String name in branchValues)
                {
                    if (nameToModule.TryGetValue(name.ToString(), out UInt32 module))
                    {
                        UInt32 blkIdx = module / 64;
                        UInt32 bitIdx = module % 64;
                        UInt64 mask = (UInt64)1 << (Int32)bitIdx;

                        Debug.Assert(blkIdx < 8);
                        unsafe
                        {
                            slotState.slot_state[blkIdx] |= mask;
                        }

                        // AddRuntimeMessage(GH_RuntimeMessageLevel.Remark,
                        //                   "Branch " + branchIndex + " (" + name + ") [" + blkIdx + "," + bitIdx + "] mask=" + mask + " slot=" + slotState);
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
        // and copy those to the FFI ready RngSeed.rng_seed fixed buffer.

        Int32 ghRandomSeed = 0;
        DA.GetData(IN_PARAM_RANDOM_SEED, ref ghRandomSeed);

        Random ghRandom = new Random(ghRandomSeed);
        byte[] rngSeedArr = new byte[16];
        ghRandom.NextBytes(rngSeedArr);

        RngSeed rngSeed;
        unsafe
        {
            for (int i = 0; i < 16; ++i)
            {
                rngSeed.rng_seed[i] = rngSeedArr[i];
            }
        }

        //
        // -- Max attempts --
        //

        Int32 maxAttemptsInt = 0;
        DA.GetData(IN_PARAM_MAX_ATTEMPTS, ref maxAttemptsInt);
        UInt32 maxAttempts = (UInt32)maxAttemptsInt;

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
                                                       rngSeed);

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
                            break;
                        case WfcWorldStateSetResult.OkNotCanonical:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Warning,
                                              "WFC solver warning: World state is not canonical");
                            break;
                        case WfcWorldStateSetResult.WorldContradictory:
                            AddRuntimeMessage(GH_RuntimeMessageLevel.Error,
                                              "WFC solver failed: World state is contradictory");
                            return;
                    }
                }
            }
        }

        UInt32 attempts = Native.wfc_observe(wfc, maxAttempts);
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
                Int64 module = Int64.MinValue;
                for (Int32 blkIdx = 0; blkIdx < 8 && module == Int64.MinValue; ++blkIdx)
                {
                    for (Int32 bitIdx = 0; bitIdx < 64 && module == Int64.MinValue; ++bitIdx)
                    {
                        UInt64 mask = (UInt64)1 << bitIdx;
                        unsafe
                        {
                            if ((slotState.slot_state[blkIdx] & mask) != 0)
                            {
                                module = (UInt32)(64 * blkIdx + bitIdx);
                            }
                        }
                    }
                }

                string name = "<unknown>";
                if (module >= 0)
                {
                    moduleToName.TryGetValue((UInt32)module, out name);
                }

                GH_Path branchPath = new GH_Path(branchIndex);
                worldStateOutput.Add(name, branchPath);

                branchIndex++;
            }
        }

        Stats stats;
        stats.ruleCount = (UInt32)minCount;
        stats.moduleCount = (UInt32)moduleToName.Count;
        stats.solveAttempts = attempts;

        DA.SetData(OUT_PARAM_DEBUG_OUTPUT, stats.ToString());
        DA.SetDataTree(OUT_PARAM_WORLD_STATE, worldStateOutput);
    }
}

internal enum AdjacencyRuleKind : UInt32
{
    X = 0,
    Y = 1,
    Z = 2,
}

[StructLayout(LayoutKind.Sequential)]
internal struct AdjacencyRule
{
    public AdjacencyRuleKind kind;
    public UInt32 module_low;
    public UInt32 module_high;
}

[StructLayout(LayoutKind.Sequential)]
internal unsafe struct RngSeed
{
    public fixed byte rng_seed[16];
}

internal enum WfcInitResult : UInt32
{
    Ok = 0,
    TooManyModules = 1,
    WorldDimensionsZero = 2,
}

internal enum WfcWorldStateSetResult : UInt32
{
    Ok = 0,
    OkNotCanonical = 1,
    WorldContradictory = 2,
}

[StructLayout(LayoutKind.Sequential)]
internal unsafe struct SlotState
{
    public fixed UInt64 slot_state[8];

    public override string ToString()
    {
        StringBuilder b = new StringBuilder("Slot state {", 128);

        b.Append("[");
        b.Append(slot_state[0]);
        b.Append("][");
        b.Append(slot_state[1]);
        b.Append("][");
        b.Append(slot_state[2]);
        b.Append("][");
        b.Append(slot_state[3]);
        b.Append("][");
        b.Append(slot_state[4]);
        b.Append("][");
        b.Append(slot_state[5]);
        b.Append("][");
        b.Append(slot_state[6]);
        b.Append("][");
        b.Append(slot_state[7]);
        b.Append("] }");

        return b.ToString();
    }
}

internal struct Stats
{
    public UInt32 ruleCount;
    public UInt32 moduleCount;
    public UInt32 solveAttempts;

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

        return b.ToString();
    }
}

internal class Native
{
    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static unsafe extern WfcInitResult wfc_init(IntPtr* wfc_ptr,
                                                         AdjacencyRule* adjacency_rules_ptr,
                                                         UIntPtr adjacency_rules_len,
                                                         UInt16 world_x,
                                                         UInt16 world_y,
                                                         UInt16 world_z,
                                                         RngSeed rng_seed);

    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static extern void wfc_free(IntPtr wfc);

    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static extern UInt32 wfc_observe(IntPtr wfc, UInt32 max_attempts);

    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static unsafe extern WfcWorldStateSetResult wfc_world_state_set(IntPtr wfc,
                                                                             SlotState* world_state_ptr,
                                                                             UIntPtr world_state_len);

    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static unsafe extern void wfc_world_state_get(IntPtr wfc,
                                                           SlotState* world_state_ptr,
                                                           UIntPtr world_state_len);
}
