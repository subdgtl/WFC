mod convert;

use std::mem;
use std::num::NonZeroU32;
use std::slice;
use std::sync::Mutex;

use wfc_core::rand::SeedableRng as _;
use wfc_core::{self, Adjacency, AdjacencyKind, ObserveResult, World};

use crate::convert::{cast_u32, cast_usize};

// FIXME: Make the `Wfc` a proper opaque handle and store the states in an
// array. https://floooh.github.io/2018/06/17/handles-vs-pointers.html

// Maximum number of modules supported to be sent with `wfc_world_state_get` and
// `wfc_world_state_set`. Sub 1 b/c because the void module occupies the first
// bit and voids don't count as modules.
const MAX_MODULE_COUNT: usize = mem::size_of::<[u64; 8]>() * 8 - 1;

#[derive(Clone, Copy)]
#[repr(u32)]
pub enum AdjacencyRuleKind {
    X = 0,
    Y = 1,
    Z = 2,
}

impl Into<AdjacencyKind> for AdjacencyRuleKind {
    fn into(self) -> AdjacencyKind {
        match self {
            Self::X => AdjacencyKind::X,
            Self::Y => AdjacencyKind::Y,
            Self::Z => AdjacencyKind::Z,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct AdjacencyRule {
    pub kind: AdjacencyRuleKind,
    pub module_low: u32,
    pub module_high: u32,
}

/// An opaque handle to `WfcState`. Actually a pointer, but shhh!
#[repr(transparent)]
pub struct Wfc(*mut WfcState);

struct WfcState(Mutex<WfcStateInner>);

struct WfcStateInner {
    // FIXME: This world_initial <-> world duplication makes the API
    // non-intuitive. On the outside it isn't obvious that you can run
    // wfc_observe multiple times without resetting the state manually.
    //
    // This could be made explicit by splitting the `Wfc` handle into two:
    // e.g. WfcCanonicalState and WfcSimState. WfcCanonicalState would contain
    // the world_initial field (world_canonical), while WfcSimState would
    // contain the world and rng fields. The init and free functions would need
    // to be duplicated between the two, e.g wfc_canonical_state_init and
    // wfc_sim_state_init. To create an instance of WfcSimState,
    // WfcCanonicalState would be required.  wfc_world_state_set would only
    // operate on WfcCanonicalstate (and would be called something like
    // wfc_canonical_state_set), and wfc_world_state_get would operate on
    // WfcSimState (and be called something like wfc_sim_state_get). wfc_observe
    // would be the way to sift state from WfcCanonicalstate to WfcSimState.
    world_initial: World,
    world: World,
    rng: rand_pcg::Pcg32,
}

impl WfcStateInner {
    /// Gets mutable borrow on every field to work around the fact that this is
    /// contained in a Mutex which has to use Deref to get fields and therefore
    /// borrows the whole struct even for field subborrows.
    fn fields_mut(&mut self) -> (&mut World, &mut World, &mut rand_pcg::Pcg32) {
        (&mut self.world_initial, &mut self.world, &mut self.rng)
    }
}

#[repr(u32)]
pub enum WfcInitResult {
    Ok = 0,
    TooManyModules = 1,
    RulesContainVoidModule = 2,
    WorldDimensionsZero = 3,
}

/// Initializes Wave Function Collapse state with adjacency rules. The world
/// gets initialized with every module possible in every slot and no voids.
///
/// To change slots to voids or change the world state, use
/// `wfc_world_state_set`.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_ptr` will be written to. It must be non-null and aligned.
///
/// - `adjacency_rules_ptr` and `adjacency_rules_len` are used to construct a
///    slice. See `slice::from_raw_parts`.
#[no_mangle]
pub unsafe extern "C" fn wfc_init(
    wfc_ptr: *mut Wfc,
    adjacency_rules_ptr: *const AdjacencyRule,
    adjacency_rules_len: usize,
    world_x: u16,
    world_y: u16,
    world_z: u16,
    rng_seed: [u8; 16],
) -> WfcInitResult {
    let adjacency_rules = {
        assert!(!adjacency_rules_ptr.is_null());
        assert_ne!(adjacency_rules_len, 0);
        assert!(adjacency_rules_len * mem::size_of::<AdjacencyRule>() < isize::MAX as usize);
        slice::from_raw_parts(adjacency_rules_ptr, adjacency_rules_len)
    };

    let mut adjacencies: Vec<Adjacency> = Vec::with_capacity(adjacency_rules.len());
    for adjacency_rule in adjacency_rules {
        if adjacency_rule.module_low == 0 || adjacency_rule.module_high == 0 {
            return WfcInitResult::RulesContainVoidModule;
        }

        let adjacency = Adjacency {
            kind: adjacency_rule.kind.into(),
            module_low: NonZeroU32::new(adjacency_rule.module_low).unwrap(),
            module_high: NonZeroU32::new(adjacency_rule.module_high).unwrap(),
        };

        adjacencies.push(adjacency);
    }

    if world_x == 0 || world_y == 0 || world_z == 0 {
        return WfcInitResult::WorldDimensionsZero;
    }

    let world_initial = World::new([world_x, world_y, world_z], adjacencies);

    if world_initial.module_count() > MAX_MODULE_COUNT {
        return WfcInitResult::TooManyModules;
    }

    let world = world_initial.clone();
    let rng = rand_pcg::Pcg32::from_seed(rng_seed);

    let wfc_state_ptr = Box::into_raw(Box::new(WfcState(Mutex::new(WfcStateInner {
        world_initial,
        world,
        rng,
    }))));
    let wfc = Wfc(wfc_state_ptr);

    assert!(!wfc_ptr.is_null());
    *wfc_ptr = wfc;

    WfcInitResult::Ok
}

/// Frees an instance of Wave Function Collapse state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc` must be a valid handle created via `wfc_init` that returned
///   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
#[no_mangle]
pub extern "C" fn wfc_free(wfc: Wfc) {
    if wfc.0.is_null() {
        return;
    }

    unsafe { Box::from_raw(wfc.0) };
}

/// Runs observations on the world until a deterministic or contradictory result
/// is found.
///
/// If contradictory result is found, the function tries again, up until
/// `max_attempts`.
///
/// Returns the number of attemps it took to find a deterministic result or zero
/// if no deterministic result was found within `max_attempts` tries.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc` must be a valid handle created via `wfc_init` that returned
///   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
#[no_mangle]
pub extern "C" fn wfc_observe(wfc: Wfc, max_attempts: u32) -> u32 {
    let wfc_state = unsafe {
        assert!(!wfc.0.is_null());
        &mut *wfc.0
    };

    let mut wfc_state_guard = wfc_state.0.lock().unwrap();
    let (world_initial, world, rng) = wfc_state_guard.fields_mut();

    let mut deterministic = false;
    let mut attempts = 0;

    while attempts < max_attempts && !deterministic {
        // Must clone on first attempt as well, because this might not be the
        // first time someone called us.
        world.clone_from(world_initial);

        let result = loop {
            let result = world.observe(rng);

            match result {
                ObserveResult::Nondeterministic => (),
                ObserveResult::Deterministic => {
                    break ObserveResult::Deterministic;
                }
                ObserveResult::Contradiction => {
                    break ObserveResult::Contradiction;
                }
            }
        };

        if result == ObserveResult::Deterministic {
            deterministic = true;
        }

        attempts += 1;
    }

    if deterministic {
        attempts
    } else {
        0
    }
}

/// Writes world state from `world_state_ptr` and `world_state_len` into the
/// provided handle.
///
/// State is stored in sparse bit vectors where each bit encodes a module
/// present at that slot, e.g. a module with 1st and 3rd bits set will contain
/// modules with ids 1 and 3. The 0th bit is reserved to denote the module is
/// void and is exclusive with all other set bits. It is a usage error to set
/// both the 0th bit and any other bit.
///
/// Currently does not validate against setting bits higher than the module
/// count, but it is a usage error to do so.
///
/// The bit vectors of state are stored in a three dimensional array (compacted
/// in a one dimensional array). To get to a slot state on position `[x, y, z]`,
/// first slice by Z, then Y, then X. E.g. for dimensions 2*2*2, the slots would
/// be in the following order:
///
/// ```text
/// [0, 0, 0]
/// [1, 0, 0]
/// [0, 1, 0]
/// [1, 1, 0]
/// [0, 0, 1]
/// [1, 0, 1]
/// [0, 1, 1]
/// [1, 1, 1]
/// ```
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc` must be a valid handle created via `wfc_init` that returned
///   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
///
/// - `world_state_ptr` and `world_state_len` are used to construct a slice. See
///   `slice::from_raw_parts`.
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_set(
    wfc: Wfc,
    world_state_ptr: *const [u64; 8],
    world_state_len: usize,
) {
    let wfc_state = {
        assert!(!wfc.0.is_null());
        &mut *wfc.0
    };

    let world_state = {
        assert!(!world_state_ptr.is_null());
        assert_ne!(world_state_len, 0);
        assert!(world_state_len * mem::size_of::<[u64; 8]>() < isize::MAX as usize);
        slice::from_raw_parts(world_state_ptr, world_state_len)
    };

    let mut wfc_state_guard = wfc_state.0.lock().unwrap();
    let (world_initial, world, _) = wfc_state_guard.fields_mut();

    import_world_state(world_initial, world_state);
    // Clone eagerly so that if someone calls `wfc_world_state_get` immediately
    // after without observing first, they get back the state that was set.
    world.clone_from(world_initial);
}

/// Reads world state from the provided handle into `world_state_ptr` and
/// `world_state_len`.
///
/// State is stored in sparse bit vectors where each bit encodes a module
/// present at that slot, e.g. a module with 1st and 3rd bits set will contain
/// modules with ids 1 and 3. The 0th bit is reserved to denote the module is
/// void and is exclusive with all other set bits.
///
/// The bit vectors of state are stored in a three dimensional array (compacted
/// in a one dimensional array). To get to a slot state on position `[x, y, z]`,
/// first slice by Z, then Y, then X. E.g. for dimensions 2*2*2, the slots would
/// be in the following order:
///
/// ```text
/// [0, 0, 0]
/// [1, 0, 0]
/// [0, 1, 0]
/// [1, 1, 0]
/// [0, 0, 1]
/// [1, 0, 1]
/// [0, 1, 1]
/// [1, 1, 1]
/// ```
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc` must be a valid handle created via `wfc_init` that returned
///   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
///
/// - `world_state_ptr` and `world_state_len` are used to construct a mutable slice. See
///   `slice::from_raw_parts_mut`.
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_get(
    wfc: Wfc,
    world_state_ptr: *mut [u64; 8],
    world_state_len: usize,
) {
    let wfc_state = {
        assert!(!wfc.0.is_null());
        &*wfc.0
    };

    let world_state = {
        assert!(!world_state_ptr.is_null());
        assert_ne!(world_state_len, 0);
        assert!(world_state_len * mem::size_of::<[u64; 8]>() < isize::MAX as usize);
        slice::from_raw_parts_mut(world_state_ptr, world_state_len)
    };

    let wfc_state_guard = wfc_state.0.lock().unwrap();
    export_world_state(&wfc_state_guard.world, world_state);
}

fn import_world_state(world: &mut World, world_state: &[[u64; 8]]) {
    let [dim_x, dim_y, dim_z] = world.dims();
    assert_eq!(
        world_state.len(),
        usize::from(dim_x) * usize::from(dim_y) * usize::from(dim_z),
    );

    for (i, slot_bits) in world_state.iter().enumerate() {
        let pos = wfc_core::index_to_position(world_state.len(), world.dims(), i);
        import_slot_state(world, pos, slot_bits);
    }
}

fn import_slot_state(world: &mut World, pos: [u16; 3], slot_state: &[u64; 8]) {
    world.set_slot_void(pos, false);
    world.set_slot_modules(pos, false);

    for (blk_index, blk) in slot_state.iter().enumerate() {
        for bit_index in 0..64 {
            let module = bit_index + 64 * blk_index;
            let value = blk & (1 << bit_index);

            if value != 0 {
                if module == 0 {
                    world.set_slot_void(pos, true);
                } else {
                    world.set_slot_module(pos, NonZeroU32::new(cast_u32(module)).unwrap(), true);
                }
            }
        }
    }
}

fn export_world_state(world: &World, world_state: &mut [[u64; 8]]) {
    let world_state_len = world_state.len();
    let [dim_x, dim_y, dim_z] = world.dims();
    assert_eq!(
        world_state_len,
        usize::from(dim_x) * usize::from(dim_y) * usize::from(dim_z),
    );

    for (i, slot_state) in world_state.iter_mut().enumerate() {
        let pos = wfc_core::index_to_position(world_state_len, world.dims(), i);
        export_slot_state(world, pos, slot_state);
    }
}

fn export_slot_state(world: &World, pos: [u16; 3], slot_state: &mut [u64; 8]) {
    static ZERO_SLOT: &[u64; 8] = &[0; 8];
    slot_state.copy_from_slice(ZERO_SLOT);

    for module in world.slot_modules_iter(pos) {
        let blk_index = cast_usize(module) / 64;
        let bit_index = cast_usize(module) % 64;

        slot_state[blk_index] |= 1 << bit_index;
    }
}
