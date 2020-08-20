using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using Grasshopper.Kernel;

public class GH_WaveFunctionCollapse3D : GH_Component
{
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
        pManager.AddIntegerParameter("World X", "X", "X dimension of the world", GH_ParamAccess.item, 5);
        pManager.AddIntegerParameter("World Y", "Y", "Y dimension of the world", GH_ParamAccess.item, 5);
        pManager.AddIntegerParameter("World Z", "Z", "Z dimension of the world", GH_ParamAccess.item, 5);

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
        pManager.AddTextParameter("out", "O", "Debug output", GH_ParamAccess.item);
        pManager.AddTextParameter("World state",
                                  "W",
                                  "State of the world once the wave function has collapsed",
                                  GH_ParamAccess.list);
    }

    protected override void SolveInstance(IGH_DataAccess DA)
    {
        //
        // -- World dimensions --
        //

        Int32 worldXInt = 0;
        Int32 worldYInt = 0;
        Int32 worldZInt = 0;
        DA.GetData("World X", ref worldXInt);
        DA.GetData("World Y", ref worldYInt);
        DA.GetData("World Z", ref worldZInt);

        if (worldXInt <= 0)
        {
            throw new ArgumentException("World X must be a positive integer");
        }
        if (worldYInt <= 0)
        {
            throw new ArgumentException("World Y must be a positive integer");
        }
        if (worldZInt <= 0)
        {
            throw new ArgumentException("World Z must be a positive integer");
        }

        UInt16 worldX = (UInt16)worldXInt;
        UInt16 worldY = (UInt16)worldYInt;
        UInt16 worldZ = (UInt16)worldZInt;

        //
        // -- Adjacency rules --
        //

        // The rules come in three lists of the same length. The first contains texts
        // representing the axis/kind (x/y/z).
        // Second and third list contain unique textual identifiers of the modules.
        // This importer replaces those string names with generated u32 numbers, starting
        // with 1 (b/c 0 is reserved for voids).

        List<string> adjacencyRulesAxis = new List<string>();
        List<string> adjacencyRulesModuleLow = new List<string>();
        List<string> adjacencyRulesModuleHigh = new List<string>();
        DA.GetDataList("Rule axis", adjacencyRulesAxis);
        DA.GetDataList("Rule low", adjacencyRulesModuleLow);
        DA.GetDataList("Rule high", adjacencyRulesModuleHigh);

        Int32 minCount = Math.Min(Math.Min(adjacencyRulesAxis.Count, adjacencyRulesModuleLow.Count),
                                  adjacencyRulesModuleHigh.Count);

        if (minCount == 0)
        {
            throw new ArgumentException("Must supply at least one adjacency rule");
        }

        UInt32 nextModule = 1;
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
        // -- Random seed --
        //

        // wfc_init needs 128 bits worth of random seed, but that is tricky to provide from GH.
        // We let GH provide an int, use it to seed a C# Random, get 16 bytes of data from that
        // and copy those to the FFI ready RngSeed.rng_seed fixed buffer.

        Int32 ghRandomSeed = 0;
        DA.GetData("Random seed", ref ghRandomSeed);

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
        DA.GetData("Max attempts", ref maxAttemptsInt);
        UInt32 maxAttempts = (UInt32)maxAttemptsInt;

        //
        // -- Initial world state --
        //

        SlotState[] worldState = new SlotState[worldX * worldY * worldZ];

        //
        // -- Run the thing and **pray** --
        //

        IntPtr wfc = IntPtr.Zero;
        unsafe
        {
            fixed (AdjacencyRule* adjacencyRulesPtr = &adjacencyRules[0])
            {
                WfcInitResult initResult = Native.wfc_init(&wfc,
                                                           adjacencyRulesPtr,
                                                           (UIntPtr)adjacencyRules.Length,
                                                           worldX,
                                                           worldY,
                                                           worldZ,
                                                           rngSeed);

                switch (initResult)
                {
                    case WfcInitResult.Ok:
                        // All good
                        break;
                    case WfcInitResult.RulesContainVoidModule:
                        throw new ArgumentException("WFC solver failed: Adjacency rules contained a void");
                    case WfcInitResult.TooManyModules:
                        throw new ArgumentException("WFC solver failed: Adjacency rules contained too many modules");
                    case WfcInitResult.WorldDimensionsZero:
                        throw new ArgumentException("WFC solver failed: World dimensions are zero");
                    default:
                        throw new ArgumentException("WFC solver failed with unknown error");
                }
            }
        }

        UInt32 attempts = Native.wfc_observe(wfc, maxAttempts);
        if (attempts == 0)
        {
            throw new ArgumentException("WFC solver failed to find solution within " + maxAttempts + " attempts");
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

        List<string> worldStateOutput = new List<string>(worldX * worldY * worldZ);
        foreach (SlotState slotState in worldState)
        {
            // Assume the result is deterministic and only take the first set bit
            UInt32 module = 0;
            for (int blkIdx = 0; blkIdx < 8 && module == 0; ++blkIdx)
            {
                for (int bitIdx = 0; bitIdx < 64 && module == 0; ++bitIdx)
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
            moduleToName.TryGetValue(module, out name);
            worldStateOutput.Add(name);
        }

        Stats stats;
        stats.ruleCount = (UInt32)minCount;
        stats.moduleCount = (UInt32)moduleToName.Count;
        stats.solveAttempts = attempts;

        DA.SetData("out", stats.ToString());
        DA.SetDataList("World state", worldStateOutput);
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
    RulesContainVoidModule = 2,
    WorldDimensionsZero = 3,
}

[StructLayout(LayoutKind.Sequential)]
internal unsafe struct SlotState
{
    public fixed UInt64 slot_state[8];
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
    internal static unsafe extern void wfc_world_state_set(IntPtr wfc,
                                                           SlotState* world_state_ptr,
                                                           UIntPtr world_state_len);

    [DllImport("wfc", CallingConvention = CallingConvention.StdCall)]
    internal static unsafe extern void wfc_world_state_get(IntPtr wfc,
                                                           SlotState* world_state_ptr,
                                                           UIntPtr world_state_len);
}
