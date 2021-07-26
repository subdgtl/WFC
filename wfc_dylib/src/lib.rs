//! # The Wave Function Collapse dynamic library C API
//!
//! None of the functions provided here are thread safe. If they are going to be
//! called from different threads, a per-handle synchronization must be
//! externally provided.

use std::mem;
use std::slice;

use wfc_core::{
    self, AdjacencyRule, Features, Rng, World, WorldNewError, WorldStatus, MAX_MODULE_COUNT,
};

/// An opaque handle to the Wave Function Collapse world state. Actually a
/// pointer, but shhh!
#[repr(transparent)]
pub struct WfcWorldStateHandle(*mut World);

/// An opaque handle to the PRNG state used by the Wave Function Collapse
/// implementation. Actually a pointer, but shhh!
#[repr(transparent)]
pub struct WfcRngStateHandle(*mut Rng);

/// Returns the maximum module count supported to be sent with
/// [`wfc_world_state_slots_get`] and [`wfc_world_state_slots_set`] by the
/// implementation.
#[no_mangle]
pub extern "C" fn wfc_query_max_module_count() -> u32 {
    u32::from(MAX_MODULE_COUNT)
}

/// Slot entropy calculation utilizes weights. Will allocate memory for weights
/// if enabled.
#[no_mangle]
pub extern "C" fn wfc_feature_weighted_entropy() -> u32 {
    Features::WEIGHTED_ENTROPY.bits()
}

/// Module selection during observation performs weighted random. Will allocate
/// memory for weights if enabled.
#[no_mangle]
pub extern "C" fn wfc_feature_weighted_observation() -> u32 {
    Features::WEIGHTED_OBSERVATION.bits()
}

#[repr(u32)]
pub enum WfcWorldStateInitResult {
    Ok = 0,
    ErrModuleCountTooHigh = 1,
    ErrWorldDimensionsZero = 2,
    ErrRulesEmpty = 3,
    ErrRulesHaveGaps = 4,
}

impl From<WorldNewError> for WfcWorldStateInitResult {
    fn from(err: WorldNewError) -> WfcWorldStateInitResult {
        match err {
            WorldNewError::ModuleCountTooHigh => Self::ErrModuleCountTooHigh,
            WorldNewError::WorldDimensionsZero => Self::ErrWorldDimensionsZero,
            WorldNewError::RulesEmpty => Self::ErrRulesEmpty,
            WorldNewError::RulesHaveGaps => Self::ErrRulesHaveGaps,
        }
    }
}

/// Creates an instance of Wave Function Collapse world state and initializes it
/// with adjacency rules. The world gets initialized with every module possible
/// in every slot.
///
/// Various [`Features`] can be enabled when creating the world. Attempting to
/// use these features without enabling them here can result in unexpected behavior.
///
/// To change the world state to a different configuration, use
/// [`wfc_world_state_slots_set`].
///
/// Initially the world is configured to have uniform weights for each module
/// across all slots, but this can be customized with
/// [`wfc_world_state_slot_module_weights_set`]. These weights can be utilized
/// either for slot entropy computation ([`Features::WEIGHTED_ENTROPY`]), or
/// weighted slot observation ([`Features::WEIGHTED_OBSERVATION`]).
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
    features: u32,
) -> WfcWorldStateInitResult {
    assert!(!wfc_world_state_handle_ptr.is_null());
    let adjacency_rules = {
        assert!(!adjacency_rules_ptr.is_null());
        assert_ne!(adjacency_rules_len, 0);
        assert!(adjacency_rules_len * mem::size_of::<AdjacencyRule>() < isize::MAX as usize);
        slice::from_raw_parts(adjacency_rules_ptr, adjacency_rules_len)
    };

    if world_x == 0 || world_y == 0 || world_z == 0 {
        return WfcWorldStateInitResult::ErrWorldDimensionsZero;
    }

    for rule in adjacency_rules {
        if rule.module_low >= MAX_MODULE_COUNT || rule.module_high >= MAX_MODULE_COUNT {
            return WfcWorldStateInitResult::ErrModuleCountTooHigh;
        }
    }

    let rules = Vec::from(adjacency_rules);

    let world_features = Features::from_bits_truncate(features);
    let world = World::new([world_x, world_y, world_z], rules, world_features);

    match world {
        Ok(world) => {
            let world_ptr = Box::into_raw(Box::new(world));
            let wfc_world_state_handle = WfcWorldStateHandle(world_ptr);

            *wfc_world_state_handle_ptr = wfc_world_state_handle;

            WfcWorldStateInitResult::Ok
        }
        Err(err) => WfcWorldStateInitResult::from(err),
    }
}

/// Creates an instance of Wave Function Collapse world state as a copy of
/// existing world state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle_ptr` will be written to. It must be non-null and
///   aligned.
///
/// - `source_wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`],
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_init_from(
    wfc_world_state_handle_ptr: *mut WfcWorldStateHandle,
    source_wfc_world_state_handle: WfcWorldStateHandle,
) {
    let source_world = {
        assert!(!source_wfc_world_state_handle.0.is_null());
        &mut *source_wfc_world_state_handle.0
    };

    let world = source_world.clone();

    let world_ptr = Box::into_raw(Box::new(world));
    let wfc_world_state_handle = WfcWorldStateHandle(world_ptr);

    assert!(!wfc_world_state_handle_ptr.is_null());
    *wfc_world_state_handle_ptr = wfc_world_state_handle;
}

// XXX: Add notion of compatibility to clone_from
/// Copies data between two instances of Wave Function Collapse world state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `destination_wfc_world_state_handle` and `source_wfc_world_state_handle`
///   must be valid handles created via [`wfc_world_state_init`] that returned
///   [`WfcWorldStateInitResult::Ok`] or [`wfc_world_state_init_from`] and not
///   yet freed via [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_clone_from(
    destination_wfc_world_state_handle: WfcWorldStateHandle,
    source_wfc_world_state_handle: WfcWorldStateHandle,
) {
    let destination_world = {
        assert!(!destination_wfc_world_state_handle.0.is_null());
        &mut *destination_wfc_world_state_handle.0
    };

    let source_world = {
        assert!(!source_wfc_world_state_handle.0.is_null());
        &*source_wfc_world_state_handle.0
    };

    destination_world.clone_from(source_world);
}

/// Frees an instance of Wave Function Collapse world state.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`],
#[no_mangle]
pub extern "C" fn wfc_world_state_free(wfc_world_state_handle: WfcWorldStateHandle) {
    if wfc_world_state_handle.0.is_null() {
        return;
    }

    unsafe { Box::from_raw(wfc_world_state_handle.0) };
}

/// XXX: Docs
pub unsafe extern "C" fn wfc_world_state_slot_module_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module: u16,
    value: u32,
) {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    // XXX: Bounds check.

    world.set_slot_module([pos_x, pos_y, pos_z], module, value > 0);
}

/// XXX: Docs
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_module_get(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module: u16,
) -> u32 {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &*wfc_world_state_handle.0
    };

    // XXX: Bounds check. Result with out var, or default value?

    let value = world.slot_module([pos_x, pos_y, pos_z], module);
    u32::from(value)
}

#[repr(u32)]
pub enum WfcWorldStateSlotModuleWeightsSetResult {
    Ok = 0,
    ErrNotNormalPositive = 1,
}

/// XXX: Docs
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_module_weights_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module_weights_ptr: *const f32,
    module_weights_len: usize,
) -> WfcWorldStateSlotModuleWeightsSetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let module_weights = {
        assert!(!module_weights_ptr.is_null());
        assert_ne!(module_weights_len, 0);
        assert!(module_weights_len * mem::size_of::<f32>() < isize::MAX as usize);
        slice::from_raw_parts(module_weights_ptr, module_weights_len)
    };

    for weight in module_weights {
        if !weight.is_normal() || !weight.is_sign_positive() {
            return WfcWorldStateSlotModuleWeightsSetResult::ErrNotNormalPositive;
        }
    }

    world.set_slot_module_weights([pos_x, pos_y, pos_z], module_weights);

    WfcWorldStateSlotModuleWeightsSetResult::Ok
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
    assert!(!wfc_rng_state_handle_ptr.is_null());

    let mut rng_seed = [0u8; 16];
    rng_seed[0..8].copy_from_slice(&rng_seed_low.to_le_bytes());
    rng_seed[8..16].copy_from_slice(&rng_seed_high.to_le_bytes());
    let rng = Rng::new(u128::from_le_bytes(rng_seed));

    let rng_ptr = Box::into_raw(Box::new(rng));
    let wfc_rng_state_handle = WfcRngStateHandle(rng_ptr);

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
pub enum WfcCanonicalizeResult {
    OkDeterministic = 0,
    OkNondeterministic = 1,
    OkContradiction = 2,
}

/// XXX: Docs
#[no_mangle]
pub unsafe extern "C" fn wfc_world_canonicalize(
    wfc_world_state_handle: WfcWorldStateHandle,
) -> WfcCanonicalizeResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let (_, world_status) = world.canonicalize();

    match world_status {
        WorldStatus::Deterministic => WfcCanonicalizeResult::OkDeterministic,
        WorldStatus::Nondeterministic => WfcCanonicalizeResult::OkNondeterministic,
        WorldStatus::Contradiction => WfcCanonicalizeResult::OkContradiction,
    }
}

#[repr(u32)]
pub enum WfcObserveResult {
    OkDeterministic = 0,
    OkNondeterministic = 1,
    OkContradiction = 2,
    ErrNotCanonical = 3,
}

/// Runs observations on the world until a deterministic or contradictory result
/// is found.
///
/// Returns [`WfcObserveResult::Deterministic`], if the world ended up in a
/// deterministic state or [`WfcObserveResult::Contradiction`] if the
/// observation made by this function created a world where a slot is occupied
/// by zero modules.
///
/// The number of performed observations can be limited if `max_observations`
/// is set to a non-zero value. For zero the number of observations remains
/// unlimited.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`],
///
/// - `wfc_rng_state_handle` must be a valid handle created via
///   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_observe(
    wfc_world_state_handle: WfcWorldStateHandle,
    wfc_rng_state_handle: WfcRngStateHandle,
    max_observations: u32,
    spent_observations: *mut u32,
) -> WfcObserveResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let rng = {
        assert!(!wfc_rng_state_handle.0.is_null());
        &mut *wfc_rng_state_handle.0
    };

    let out_observations = {
        assert!(!spent_observations.is_null());
        &mut *spent_observations
    };

    if world.slots_modified() {
        return WfcObserveResult::ErrNotCanonical;
    }

    if max_observations == 0 {
        return match world.world_status() {
            WorldStatus::Deterministic => WfcObserveResult::OkDeterministic,
            WorldStatus::Nondeterministic => WfcObserveResult::OkNondeterministic,
            WorldStatus::Contradiction => WfcObserveResult::OkContradiction,
        };
    }

    let mut observations = 0;
    loop {
        let (_, status) = world.observe(rng);
        observations += 1;

        match status {
            WorldStatus::Deterministic => {
                *out_observations = observations;
                return WfcObserveResult::OkDeterministic;
            }
            WorldStatus::Nondeterministic => {
                if observations == max_observations {
                    *out_observations = observations;
                    return WfcObserveResult::OkNondeterministic;
                }
            }
            WorldStatus::Contradiction => {
                *out_observations = observations;
                return WfcObserveResult::OkContradiction;
            }
        }
    }
}
