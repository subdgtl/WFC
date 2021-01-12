//! # The Wave Function Collapse dynamic library C API
//!
//! None of the functions provided here are thread safe. If they are going to be
//! called from different threads, a per-handle synchronization must be
//! externally provided.

mod convert;

use std::mem;
use std::slice;

use wfc_core::rand_core::SeedableRng as _;
use wfc_core::{self, Adjacency, AdjacencyKind, World, WorldStatus};

use crate::convert::{cast_u8, cast_usize};

/// Maximum number of modules supported to be sent with
/// [`wfc_world_state_slots_get`] and [`wfc_world_state_slots_set`].
const MAX_MODULE_COUNT: usize = mem::size_of::<[u64; 4]>() * 8;

#[repr(u32)]
#[derive(Clone, Copy)]
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
    pub module_low: u8,
    pub module_high: u8,
}

impl Into<Adjacency> for AdjacencyRule {
    fn into(self) -> Adjacency {
        Adjacency {
            kind: self.kind.into(),
            module_low: self.module_low,
            module_high: self.module_high,
        }
    }
}

/// An opaque handle to the Wave Function Collapse world state. Actually a
/// pointer, but shhh!
#[repr(transparent)]
pub struct WfcWorldStateHandle(*mut World);

/// An opaque handle to the PRNG state used by the Wave Function Collapse
/// implementation. Actually a pointer, but shhh!
#[repr(transparent)]
pub struct WfcRngStateHandle(*mut rand_pcg::Pcg32);

#[repr(u32)]
pub enum WfcWorldStateInitResult {
    Ok = 0,
    ErrTooManyModules = 1,
    ErrWorldDimensionsZero = 2,
}

/// Creates an instance of Wave Function Collapse world state and initializes it
/// with adjacency rules. The world gets initialized with every module possible
/// in every slot.
///
/// To change the world state to a different configuration, use
/// [`wfc_world_state_slots_set`].
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle_ptr` will be written to. It must be non-null and
///   aligned,
///
/// - `adjacency_rules_ptr` and `adjacency_rules_len` are used to construct a
///   slice. See [`std::slice::from_raw_parts`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_init(
    wfc_world_state_handle_ptr: *mut WfcWorldStateHandle,
    adjacency_rules_ptr: *const AdjacencyRule,
    adjacency_rules_len: usize,
    world_x: u16,
    world_y: u16,
    world_z: u16,
) -> WfcWorldStateInitResult {
    let adjacency_rules = {
        assert!(!adjacency_rules_ptr.is_null());
        assert_ne!(adjacency_rules_len, 0);
        assert!(adjacency_rules_len * mem::size_of::<AdjacencyRule>() < isize::MAX as usize);
        slice::from_raw_parts(adjacency_rules_ptr, adjacency_rules_len)
    };

    if world_x == 0 || world_y == 0 || world_z == 0 {
        return WfcWorldStateInitResult::ErrWorldDimensionsZero;
    }

    let adjacencies = adjacency_rules
        .iter()
        .map(|adjacency_rule| (*adjacency_rule).into())
        .collect();
    let world = World::new([world_x, world_y, world_z], adjacencies);

    if world.module_count() > MAX_MODULE_COUNT {
        return WfcWorldStateInitResult::ErrTooManyModules;
    }

    let world_ptr = Box::into_raw(Box::new(world));
    let wfc_world_state_handle = WfcWorldStateHandle(world_ptr);

    assert!(!wfc_world_state_handle_ptr.is_null());
    *wfc_world_state_handle_ptr = wfc_world_state_handle;

    WfcWorldStateInitResult::Ok
}

/// Frees an instance of Wave Function Collapse world state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] and
///   not yet freed via [`wfc_world_state_free`],
#[no_mangle]
pub extern "C" fn wfc_world_state_free(wfc_world_state_handle: WfcWorldStateHandle) {
    if wfc_world_state_handle.0.is_null() {
        return;
    }

    unsafe { Box::from_raw(wfc_world_state_handle.0) };
}

#[repr(u32)]
pub enum WfcWorldStateSlotsSetResult {
    Ok = 0,
    OkWorldNotCanonical = 1,
    ErrWorldContradictory = 2,
}

/// Writes Wave Function Collapse slots from `slots_ptr` and `slots_len` into
/// the provided handle.
///
/// Slots are stored in sparse bit vectors where each bit encodes a module
/// present at that slot, e.g. a slot with 0th and 2nd bits set will contain
/// modules with ids 0 and 2.
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
/// If this function returns
/// [`WfcWorldStateSlotsSetResult::ErrWorldContradictory`], the provided handle
/// becomes invalid. It will become valid once again when passed to this
/// function and [`WfcWorldStateSlotsSetResult::Ok`] or
/// [`WfcWorldStateSlotsSetResult::OkWorldNotCanonical`] is returned.
///
/// If the modules in the provided slots could still be collapsed according to
/// the current rule set, the world is not canonical. This function fixes that
/// and returns [`WfcWorldStateSlotsSetResult::OkWorldNotCanonical`] as a
/// warning.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] and
///   not yet freed via [`wfc_world_state_free`],
///
/// - `slots_ptr` and `slots_len` are used to construct a slice. See
///   [`std::slice::from_raw_parts`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slots_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    slots_ptr: *const [u64; 4],
    slots_len: usize,
) -> WfcWorldStateSlotsSetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let slots = {
        assert!(!slots_ptr.is_null());
        assert_ne!(slots_len, 0);
        assert!(slots_len * mem::size_of::<[u64; 4]>() < isize::MAX as usize);
        slice::from_raw_parts(slots_ptr, slots_len)
    };

    import_slots(world, slots);

    // Since we are importing custom world state, we can not be sure all
    // adjacency rule constraints are initially satisfied.
    let (world_changed, world_status) = world.ensure_constraints();
    match (world_changed, world_status) {
        (_, WorldStatus::Contradiction) => WfcWorldStateSlotsSetResult::ErrWorldContradictory,
        (true, _) => WfcWorldStateSlotsSetResult::Ok,
        (false, _) => WfcWorldStateSlotsSetResult::OkWorldNotCanonical,
    }
}

/// Reads slots from the provided handle into `slots_ptr` and `slots_len`.
///
/// State is stored in sparse bit vectors where each bit encodes a module
/// present at that slot, e.g. a slot with 0th and 2nd bits set will contain
/// modules with ids 0 and 2.
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
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] and
///   not yet freed via [`wfc_world_state_free`],
///
/// - `slots_ptr` and `slots_len` are used to construct a mutable slice. See
///   [`std::slice::from_raw_parts_mut`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slots_get(
    wfc_world_state_handle: WfcWorldStateHandle,
    slots_ptr: *mut [u64; 4],
    slots_len: usize,
) {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &*wfc_world_state_handle.0
    };

    let slots = {
        assert!(!slots_ptr.is_null());
        assert_ne!(slots_len, 0);
        assert!(slots_len * mem::size_of::<[u64; 4]>() < isize::MAX as usize);
        slice::from_raw_parts_mut(slots_ptr, slots_len)
    };

    export_slots(world, slots);
}

/// Creates an instance of pseudo-random number generator and initializes it
/// with the provided seed.
///
/// The PRNG used requires 128 bits of random seed. It is provided as two 64 bit
/// unsigned integers: `rng_seed_low` and `rng_seed_high`. They are expected to
/// be little-endian on all platforms.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_rng_state_handle_ptr` will be written to. It must be non-null and
///   aligned.
#[no_mangle]
pub unsafe extern "C" fn wfc_rng_state_init(
    wfc_rng_state_handle_ptr: *mut WfcRngStateHandle,
    rng_seed_low: u64,
    rng_seed_high: u64,
) {
    let mut rng_seed = [0u8; 16];
    rng_seed[0..8].copy_from_slice(&rng_seed_low.to_le_bytes());
    rng_seed[8..16].copy_from_slice(&rng_seed_high.to_le_bytes());
    let rng = rand_pcg::Pcg32::from_seed(rng_seed);

    let rng_ptr = Box::into_raw(Box::new(rng));
    let wfc_rng_state_handle = WfcRngStateHandle(rng_ptr);

    assert!(!wfc_rng_state_handle_ptr.is_null());
    *wfc_rng_state_handle_ptr = wfc_rng_state_handle;
}

/// Frees an instance of Wave Function Collapse RNG state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_rng_state_handle` must be a valid handle created via
///   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`].
#[no_mangle]
pub extern "C" fn wfc_rng_state_free(wfc_rng_state_handle: WfcRngStateHandle) {
    if wfc_rng_state_handle.0.is_null() {
        return;
    }

    unsafe { Box::from_raw(wfc_rng_state_handle.0) };
}

#[repr(u32)]
pub enum WfcObserveResult {
    Deterministic = 0,
    Contradiction = 1,
}

/// Runs observations on the world until a deterministic or contradictory result
/// is found.
///
/// Returns [`WfcObserveResult::Deterministic`], if the world ended up in a
/// deterministic state or [`WfcObserveResult::Contradiction`] if the
/// observation made by this function created a world where a slot is occupied
/// by zero modules.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] and
///   not yet freed via [`wfc_world_state_free`],
///
/// - `wfc_rng_state_handle` must be a valid handle created via
///   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`].
#[no_mangle]
pub extern "C" fn wfc_observe(
    wfc_world_state_handle: WfcWorldStateHandle,
    wfc_rng_state_handle: WfcRngStateHandle,
) -> WfcObserveResult {
    let world = unsafe {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };
    let rng = unsafe {
        assert!(!wfc_rng_state_handle.0.is_null());
        &mut *wfc_rng_state_handle.0
    };

    loop {
        let (_, status) = world.observe(rng);

        match status {
            WorldStatus::Nondeterministic => (),
            WorldStatus::Deterministic => {
                return WfcObserveResult::Deterministic;
            }
            WorldStatus::Contradiction => {
                return WfcObserveResult::Contradiction;
            }
        }
    }
}

fn import_slots(world: &mut World, world_state: &[[u64; 4]]) {
    let [dim_x, dim_y, dim_z] = world.dims();
    assert_eq!(
        world_state.len(),
        usize::from(dim_x) * usize::from(dim_y) * usize::from(dim_z),
    );

    for (i, slot_bits) in world_state.iter().enumerate() {
        let pos = wfc_core::index_to_position(world_state.len(), world.dims(), i);
        import_slot(world, pos, slot_bits);
    }
}

fn import_slot(world: &mut World, pos: [u16; 3], slot_state: &[u64; 4]) {
    world.set_slot_modules(pos, false);

    for (blk_index, blk) in slot_state.iter().enumerate() {
        for bit_index in 0..64 {
            let module = bit_index + 64 * blk_index;
            let value = blk & (1 << bit_index);

            if value != 0 {
                world.set_slot_module(pos, cast_u8(module), true);
            }
        }
    }
}

fn export_slots(world: &World, world_state: &mut [[u64; 4]]) {
    let world_state_len = world_state.len();
    let [dim_x, dim_y, dim_z] = world.dims();
    assert_eq!(
        world_state_len,
        usize::from(dim_x) * usize::from(dim_y) * usize::from(dim_z),
    );

    for (i, slot_state) in world_state.iter_mut().enumerate() {
        let pos = wfc_core::index_to_position(world_state_len, world.dims(), i);
        export_slot(world, pos, slot_state);
    }
}

fn export_slot(world: &World, pos: [u16; 3], slot_state: &mut [u64; 4]) {
    static ZERO_SLOT: &[u64; 4] = &[0; 4];
    slot_state.copy_from_slice(ZERO_SLOT);

    for module in world.slot_modules_iter(pos) {
        let blk_index = cast_usize(module) / 64;
        let bit_index = cast_usize(module) % 64;

        slot_state[blk_index] |= 1 << bit_index;
    }
}
