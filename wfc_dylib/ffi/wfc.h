#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum WfcCanonicalizeResult {
  OkDeterministic = 0,
  OkNondeterministic = 1,
  OkContradiction = 2,
};
typedef uint32_t WfcCanonicalizeResult;

enum WfcObserveResult {
  OkDeterministic = 0,
  OkNondeterministic = 1,
  OkContradiction = 2,
  ErrNotCanonical = 3,
};
typedef uint32_t WfcObserveResult;

enum WfcWorldStateCloneFromResult {
  Ok = 0,
  ErrIncompatible = 1,
};
typedef uint32_t WfcWorldStateCloneFromResult;

enum WfcWorldStateInitResult {
  Ok = 0,
  ErrModuleCountTooHigh = 1,
  ErrWorldDimensionsZero = 2,
  ErrRulesEmpty = 3,
  ErrRulesHaveGaps = 4,
};
typedef uint32_t WfcWorldStateInitResult;

enum WfcWorldStateSlotModuleGetResult {
  Ok = 0,
  ErrOutOfBounds = 1,
};
typedef uint32_t WfcWorldStateSlotModuleGetResult;

enum WfcWorldStateSlotModuleSetResult {
  Ok = 0,
  ErrOutOfBounds = 1,
};
typedef uint32_t WfcWorldStateSlotModuleSetResult;

enum WfcWorldStateSlotModuleWeightsSetResult {
  Ok = 0,
  ErrOutOfBounds = 1,
  ErrWeightsLengthMismatch = 2,
  ErrWeightsNotNormalPositive = 3,
};
typedef uint32_t WfcWorldStateSlotModuleWeightsSetResult;

/**
 * An opaque handle to the Wave Function Collapse world state. Actually a
 * pointer, but shhh!
 */
typedef World *WfcWorldStateHandle;

/**
 * An opaque handle to the PRNG state used by the Wave Function Collapse
 * implementation. Actually a pointer, but shhh!
 */
typedef Rng *WfcRngStateHandle;

/**
 * Returns the maximum module count supported to be sent with
 * [`wfc_world_state_slot_module_get`] and [`wfc_world_state_slot_module_set`]
 * by the implementation.
 */
uint32_t wfc_query_max_module_count(void);

/**
 * Slot entropy calculation utilizes weights. Will allocate memory for weights
 * if enabled.
 */
uint32_t wfc_feature_weighted_entropy(void);

/**
 * Module selection during observation performs weighted random. Will allocate
 * memory for weights if enabled.
 */
uint32_t wfc_feature_weighted_observation(void);

/**
 * Creates an instance of Wave Function Collapse world state and initializes it
 * with adjacency rules. The world gets initialized with every module possible
 * in every slot.
 *
 * Various [`Features`] can be enabled when creating the world. Attempting to
 * use these features without enabling them here can result in unexpected behavior.
 *
 * To change the world state to a different configuration, use
 * [`wfc_world_state_slot_module_set`].
 *
 * Initially the world is configured to have uniform weights for each module
 * across all slots, but this can be customized with
 * [`wfc_world_state_slot_module_weights_set`]. These weights can be utilized
 * either for slot entropy computation ([`Features::WEIGHTED_ENTROPY`]), or
 * weighted slot observation ([`Features::WEIGHTED_OBSERVATION`]).
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle_ptr` will be written to. It must be non-null and
 *   aligned,
 *
 * - `adjacency_rules_ptr` and `adjacency_rules_len` are used to construct a
 *   slice. See [`std::slice::from_raw_parts`].
 */
WfcWorldStateInitResult wfc_world_state_init(WfcWorldStateHandle *wfc_world_state_handle_ptr,
                                             const AdjacencyRule *adjacency_rules_ptr,
                                             uintptr_t adjacency_rules_len,
                                             uint16_t world_x,
                                             uint16_t world_y,
                                             uint16_t world_z,
                                             uint32_t features);

/**
 * Creates an instance of Wave Function Collapse world state as a copy of
 * existing world state.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle_ptr` will be written to. It must be non-null and
 *   aligned,
 *
 * - `source_wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`].
 */
void wfc_world_state_init_from(WfcWorldStateHandle *wfc_world_state_handle_ptr,
                               WfcWorldStateHandle source_wfc_world_state_handle);

/**
 * Copies data between two instances of Wave Function Collapse world state, if
 * compatible.
 *
 * Compatibility is determined by comparing the internal block size of the slot
 * storage, i.e. whether both world states have the capability store the same
 * amount modules in a slot. Worlds created from same parameters are always
 * compatible, as are worlds created from other worlds as blueprints via
 * [`wfc_world_state_init_from`].
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `destination_wfc_world_state_handle` and `source_wfc_world_state_handle`
 *   must be valid handles created via [`wfc_world_state_init`] that returned
 *   [`WfcWorldStateInitResult::Ok`] or [`wfc_world_state_init_from`] and not
 *   yet freed via [`wfc_world_state_free`].
 */
WfcWorldStateCloneFromResult wfc_world_state_clone_from(WfcWorldStateHandle destination_wfc_world_state_handle,
                                                        WfcWorldStateHandle source_wfc_world_state_handle);

/**
 * Frees an instance of Wave Function Collapse world state.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`].
 */
void wfc_world_state_free(WfcWorldStateHandle wfc_world_state_handle);

/**
 * Stores one Wave Function Collapse module into a slot of the provided handle.
 *
 * Nonzero values count as `true`.
 *
 * Setting a slot changes the world from canonical to modified state, meaning
 * the library does not know for certain if all WFC constraints are upheld. It
 * is an error to observe a modified world. In order to observe this world
 * state handle again, call [`wfc_world_canonicalize`], which will transition
 * the world back to canonical state (and potentially remove some modules from
 * slots as a result).
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`].
 */
WfcWorldStateSlotModuleSetResult wfc_world_state_slot_module_set(WfcWorldStateHandle wfc_world_state_handle,
                                                                 uint16_t pos_x,
                                                                 uint16_t pos_y,
                                                                 uint16_t pos_z,
                                                                 uint16_t module,
                                                                 uint32_t value);

/**
 * Loads one Wave Function Collapse module from a slot of the provided handle
 * into `value`.
 *
 * If the module is present, the value will be 1, otherwise 0.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`],
 *
 * - `value` must be a non-null, aligned pointer to a [`u32`].
 */
WfcWorldStateSlotModuleGetResult wfc_world_state_slot_module_get(WfcWorldStateHandle wfc_world_state_handle,
                                                                 uint16_t pos_x,
                                                                 uint16_t pos_y,
                                                                 uint16_t pos_z,
                                                                 uint16_t module,
                                                                 uint32_t *value);

/**
 * Stores weights for one Wave Function Collapse slot into the provided handle.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`],
 *
 * - `module_weights_ptr` and `module_weights_len` are used to construct a
 *   slice. See [`std::slice::from_raw_parts`].
 */
WfcWorldStateSlotModuleWeightsSetResult wfc_world_state_slot_module_weights_set(WfcWorldStateHandle wfc_world_state_handle,
                                                                                uint16_t pos_x,
                                                                                uint16_t pos_y,
                                                                                uint16_t pos_z,
                                                                                const float *module_weights_ptr,
                                                                                uintptr_t module_weights_len);

/**
 * Creates an instance of pseudo-random number generator and initializes it
 * with the provided seed.
 *
 * The PRNG used requires 128 bits of random seed. It is provided as two 64 bit
 * unsigned integers: `rng_seed_low` and `rng_seed_high`. They are expected to
 * be little-endian on all platforms.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_rng_state_handle_ptr` will be written to. It must be non-null and
 *   aligned.
 */
void wfc_rng_state_init(WfcRngStateHandle *wfc_rng_state_handle_ptr,
                        uint64_t rng_seed_low,
                        uint64_t rng_seed_high);

/**
 * Frees an instance of Wave Function Collapse RNG state.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_rng_state_handle` must be a valid handle created via
 *   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`].
 */
void wfc_rng_state_free(WfcRngStateHandle wfc_rng_state_handle);

/**
 * Canonicalizes a Wave Function Collapse world.
 *
 * While the initially created world starts in a canonical state, setting slots
 * with [`wfc_world_state_slot_module_set`] can invalidate that. Checking this
 * is expensive, and therefore the library just pessimistically transitions the
 * world to a modified state.
 *
 * Once all the desired state modifications are applied, use
 * [`wfc_world_canonicalize`] for this handle to be once again usable with
 * [`wfc_observe`].
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`].
 */
WfcCanonicalizeResult wfc_world_canonicalize(WfcWorldStateHandle wfc_world_state_handle);

/**
 * Runs observations on the world until a deterministic or contradictory result
 * is found.
 *
 * Returns [`WfcObserveResult::Deterministic`], if the world ended up in a
 * deterministic state or [`WfcObserveResult::Contradiction`] if the
 * observation made by this function created a world where a slot is occupied
 * by zero modules.
 *
 * The number of performed observations can be limited if `max_observations`
 * is set to a non-zero value. For zero the number of observations remains
 * unlimited.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`],
 *
 * - `wfc_rng_state_handle` must be a valid handle created via
 *   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`],
 *
 * - `spent_observations` must be a non-null, aligned pointer to a [`u32`].
 */
WfcObserveResult wfc_observe(WfcWorldStateHandle wfc_world_state_handle,
                             WfcRngStateHandle wfc_rng_state_handle,
                             uint32_t max_observations,
                             uint32_t *spent_observations);
