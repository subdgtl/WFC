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
/// [`wfc_world_state_slot_module_get`] and [`wfc_world_state_slot_module_set`]
/// by the implementation.
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

/// Some slots can be masked-out (disabled). Will allocate memory for the mask.
#[no_mangle]
pub extern "C" fn wfc_feature_masked_slots() -> u32 {
    Features::MASKED_SLOTS.bits()
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
/// [`wfc_world_state_slot_module_set`].
///
/// Initially the world is configured to have uniform weights for each module
/// across all slots, but this can be customized with
/// [`wfc_world_state_slot_module_weight_set`]. These weights can be utilized
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
    wfc_world_state_init_ex(
        wfc_world_state_handle_ptr,
        adjacency_rules_ptr,
        adjacency_rules_len,
        world_x,
        world_y,
        world_z,
        features,
        1,
        1.0,
        0,
    )
}

/// This is exactly like [`wfc_world_state_init`], but allows for setting
/// initial values for slots, slot module weights and slot masks.
///
/// `slot_init_value` is a 32-bit boolean, either 0 or 1. If 0, the slot will
/// start with all modules unset. If 1, all supported modules will be set. The
/// value can be further customized with [`wfc_world_state_slot_module_set`].
///
/// `slot_module_weight_init_value` is a 32-bit floating point number all slot
/// module weights will be initialized to. Weights can be further customized
/// with [`wfc_world_state_slot_module_weight_set`]. The value is ignored
/// neither [`Features::WEIGHTED_OBSERVATION`] nor
/// [`Features::WEIGHTED_ENTROPY`] is passed.
///
/// `slot_mask_init_value` is a 32-bit boolean, either 0 or 1. All slot masks
/// are initialized to this value. 1 means the slot is enabled, 0 means the slot
/// is ignored. The value is ignored if [`Features::MASKED_SLOTS`] is not
/// passed.
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_init_ex(
    wfc_world_state_handle_ptr: *mut WfcWorldStateHandle,
    adjacency_rules_ptr: *const AdjacencyRule,
    adjacency_rules_len: usize,
    world_x: u16,
    world_y: u16,
    world_z: u16,
    features: u32,
    slot_init_value: u32,
    slot_module_weight_init_value: f32,
    slot_mask_init_value: u32,
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
    let world = World::new(
        [world_x, world_y, world_z],
        rules,
        world_features,
        slot_init_value > 0,
        slot_module_weight_init_value,
        slot_mask_init_value > 0,
    );

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
///   aligned,
///
/// - `source_wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`].
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

#[repr(u32)]
pub enum WfcWorldStateCloneFromResult {
    Ok = 0,
    ErrIncompatible = 1,
}

/// Copies data between two instances of Wave Function Collapse world state, if
/// compatible.
///
/// Compatibility is determined by comparing the internal block size of the slot
/// storage, i.e. whether both world states have the capability store the same
/// amount modules in a slot. Worlds created from same parameters are always
/// compatible, as are worlds created from other worlds as blueprints via
/// [`wfc_world_state_init_from`].
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
) -> WfcWorldStateCloneFromResult {
    let destination_world = {
        assert!(!destination_wfc_world_state_handle.0.is_null());
        &mut *destination_wfc_world_state_handle.0
    };

    let source_world = {
        assert!(!source_wfc_world_state_handle.0.is_null());
        &*source_wfc_world_state_handle.0
    };

    if !destination_world.clone_compatible(source_world) {
        return WfcWorldStateCloneFromResult::ErrIncompatible;
    }

    destination_world.clone_from(source_world);

    WfcWorldStateCloneFromResult::Ok
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
///   [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_free(wfc_world_state_handle: WfcWorldStateHandle) {
    if wfc_world_state_handle.0.is_null() {
        return;
    }

    Box::from_raw(wfc_world_state_handle.0);
}

#[repr(u32)]
pub enum WfcWorldStateSlotModuleSetResult {
    Ok = 0,
    ErrSlotOutOfBounds = 1,
    ErrModuleOutOfBounds = 2,
}

/// Stores one Wave Function Collapse module into a slot of the provided handle.
///
/// Nonzero values of `module_is_set` count as `true`.
///
/// Setting a slot changes the world from canonical to modified state, meaning
/// the library does not know for certain if all WFC constraints are upheld. It
/// is an error to observe a modified world. In order to observe this world
/// state handle again, call [`wfc_world_canonicalize`], which will transition
/// the world back to canonical state (and potentially remove some modules from
/// slots as a result).
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_module_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module: u16,
    module_is_set: u32,
) -> WfcWorldStateSlotModuleSetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let [dim_x, dim_y, dim_z] = world.dims();
    if pos_x >= dim_x || pos_y >= dim_y || pos_z >= dim_z {
        return WfcWorldStateSlotModuleSetResult::ErrSlotOutOfBounds;
    }

    if module >= world.module_count() {
        return WfcWorldStateSlotModuleSetResult::ErrModuleOutOfBounds;
    }

    world.set_slot_module([pos_x, pos_y, pos_z], module, module_is_set > 0);

    WfcWorldStateSlotModuleSetResult::Ok
}

#[repr(u32)]
pub enum WfcWorldStateSlotModuleGetResult {
    Ok = 0,
    ErrSlotOutOfBounds = 1,
    ErrModuleOutOfBounds = 2,
}

/// Loads one Wave Function Collapse module from a slot of the provided handle
/// into `module_is_set`.
///
/// If the module is present, the value will be 1, otherwise 0.
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
/// - `module_is_set` must be a non-null, aligned pointer to a [`u32`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_module_get(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module: u16,
    module_is_set: *mut u32,
) -> WfcWorldStateSlotModuleGetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &*wfc_world_state_handle.0
    };

    let out_module_is_set = {
        assert!(!module_is_set.is_null());
        &mut *module_is_set
    };

    let [dim_x, dim_y, dim_z] = world.dims();
    if pos_x >= dim_x || pos_y >= dim_y || pos_z >= dim_z {
        return WfcWorldStateSlotModuleGetResult::ErrSlotOutOfBounds;
    }

    if module >= world.module_count() {
        return WfcWorldStateSlotModuleGetResult::ErrModuleOutOfBounds;
    }

    if world.slot_module([pos_x, pos_y, pos_z], module) {
        *out_module_is_set = 1;
    } else {
        *out_module_is_set = 0;
    }

    WfcWorldStateSlotModuleGetResult::Ok
}

#[repr(u32)]
pub enum WfcWorldStateSlotModuleWeightSetResult {
    Ok = 0,
    ErrSlotOutOfBounds = 1,
    ErrModuleOutOfBounds = 2,
    ErrWeightNotNormalPositive = 3,
}

/// Stores a weight for one Wave Function Collapse slot into the provided handle.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_module_weight_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    module: u16,
    weight: f32,
) -> WfcWorldStateSlotModuleWeightSetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let [dim_x, dim_y, dim_z] = world.dims();
    if pos_x >= dim_x || pos_y >= dim_y || pos_z >= dim_z {
        return WfcWorldStateSlotModuleWeightSetResult::ErrSlotOutOfBounds;
    }

    if module >= world.module_count() {
        return WfcWorldStateSlotModuleWeightSetResult::ErrModuleOutOfBounds;
    }

    if !weight.is_normal() || !weight.is_sign_positive() {
        return WfcWorldStateSlotModuleWeightSetResult::ErrWeightNotNormalPositive;
    }

    world.set_slot_module_weight([pos_x, pos_y, pos_z], module, weight);

    WfcWorldStateSlotModuleWeightSetResult::Ok
}

#[repr(u32)]
pub enum WfcWorldStateSlotMaskSetResult {
    Ok = 0,
    ErrSlotOutOfBounds = 1,
}

/// Stores a mask for one Wave Function Collapse slot into the provided
/// handle. Mask is a 32-bit boolean value, either 0 or 1. 1 means the slot is
/// enabled, 0 means the slot is ignored.
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_slot_mask_set(
    wfc_world_state_handle: WfcWorldStateHandle,
    pos_x: u16,
    pos_y: u16,
    pos_z: u16,
    mask: u32,
) -> WfcWorldStateSlotMaskSetResult {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let [dim_x, dim_y, dim_z] = world.dims();
    if pos_x >= dim_x || pos_y >= dim_y || pos_z >= dim_z {
        return WfcWorldStateSlotMaskSetResult::ErrSlotOutOfBounds;
    }

    world.set_slot_mask([pos_x, pos_y, pos_z], mask > 0);

    WfcWorldStateSlotMaskSetResult::Ok
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
pub unsafe extern "C" fn wfc_rng_state_free(wfc_rng_state_handle: WfcRngStateHandle) {
    if wfc_rng_state_handle.0.is_null() {
        return;
    }

    Box::from_raw(wfc_rng_state_handle.0);
}

/// Canonicalizes a Wave Function Collapse world.
///
/// While the initially created world starts in a canonical state, setting slots
/// with [`wfc_world_state_slot_module_set`] can invalidate that. Checking this
/// is expensive, and therefore the library just pessimistically transitions the
/// world to a modified state.
///
/// Once all the desired state modifications are applied, use
/// [`wfc_world_canonicalize`] for this handle to be once again usable with
/// [`wfc_observe`].
///
/// # Safety
///
/// Behavior is undefined if any of the following conditions are violated:
///
/// - `wfc_world_state_handle` must be a valid handle created via
///   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
///   [`wfc_world_state_init_from`] and not yet freed via
///   [`wfc_world_state_free`].
#[no_mangle]
pub unsafe extern "C" fn wfc_world_state_canonicalize(
    wfc_world_state_handle: WfcWorldStateHandle,
    world_status: *mut WorldStatus,
) {
    let world = {
        assert!(!wfc_world_state_handle.0.is_null());
        &mut *wfc_world_state_handle.0
    };

    let out_world_status = {
        assert!(!world_status.is_null());
        &mut *world_status
    };

    let (_, status) = world.canonicalize();

    *out_world_status = status;
}

#[repr(u32)]
pub enum WfcObserveResult {
    Ok = 0,
    ErrNotCanonical = 1,
}

/// Runs observations on the world until a deterministic or contradictory result
/// is found.
///
/// The number of performed observations can be limited if `max_observations`
/// is set to a non-zero value. For zero the number of observations remains
/// unlimited.
///
/// Outputs [`WorldStatus::Deterministic`], if the world ended up in a
/// deterministic state or [`WorldStatus::Contradiction`] if the observation
/// made by this function created a world where a slot is occupied by zero
/// modules. [`WorldStatus::Nondeterministic`] can be outputted if the
/// observation limit took effect sooner than the world could become
/// deterministic or contradictory.
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
///   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`],
///
/// - `spent_observations` must be a non-null, aligned pointer to a [`u32`].
#[no_mangle]
pub unsafe extern "C" fn wfc_observe(
    wfc_world_state_handle: WfcWorldStateHandle,
    wfc_rng_state_handle: WfcRngStateHandle,
    max_observations: u32,
    spent_observations: *mut u32,
    world_status: *mut WorldStatus,
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

    let out_world_status = {
        assert!(!world_status.is_null());
        &mut *world_status
    };

    if world.slots_modified() {
        return WfcObserveResult::ErrNotCanonical;
    }

    if max_observations == 0 {
        let status = world.world_status();
        *out_world_status = status;
    }

    let mut observations = 0;
    loop {
        let (_, status) = world.observe(rng);
        observations += 1;

        match status {
            WorldStatus::Deterministic => {
                *out_observations = observations;
                *out_world_status = status;

                return WfcObserveResult::Ok;
            }
            WorldStatus::Nondeterministic => {
                if observations == max_observations {
                    *out_observations = observations;
                    *out_world_status = status;

                    return WfcObserveResult::Ok;
                }
            }
            WorldStatus::Contradiction => {
                *out_observations = observations;
                *out_world_status = status;

                return WfcObserveResult::Ok;
            }
        }
    }
}
