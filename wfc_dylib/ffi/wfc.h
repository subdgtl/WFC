#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum AdjacencyRuleKind {
  X = 0,
  Y = 1,
  Z = 2,
};
typedef uint32_t AdjacencyRuleKind;

enum Entropy {
  Linear = 0,
  Shannon = 1,
};
typedef uint32_t Entropy;

enum WfcObserveResult {
  Deterministic = 0,
  Contradiction = 1,
  Nondeterministic = 2,
};
typedef uint32_t WfcObserveResult;

enum WfcWorldStateInitResult {
  Ok = 0,
  ErrTooManyModules = 1,
  ErrWorldDimensionsZero = 2,
};
typedef uint32_t WfcWorldStateInitResult;

enum WfcWorldStateSlotsSetResult {
  Ok = 0,
  OkWorldNotCanonical = 1,
  ErrWorldContradictory = 2,
};
typedef uint32_t WfcWorldStateSlotsSetResult;

/**
 * An opaque handle to the Wave Function Collapse world state. Actually a
 * pointer, but shhh!
 */
typedef World *WfcWorldStateHandle;

typedef struct AdjacencyRule {
  AdjacencyRuleKind kind;
  uint8_t module_low;
  uint8_t module_high;
} AdjacencyRule;

/**
 * An opaque handle to the PRNG state used by the Wave Function Collapse
 * implementation. Actually a pointer, but shhh!
 */
typedef Pcg32 *WfcRngStateHandle;

/**
 * Returns the maximum module count supported to be sent with
 * [`wfc_world_state_slots_get`] and [`wfc_world_state_slots_set`] by the
 * implementation.
 */
uint32_t wfc_max_module_count_get(void);

/**
 * Creates an instance of Wave Function Collapse world state and initializes it
 * with adjacency rules. The world gets initialized with every module possible
 * in every slot.
 *
 * To change the world state to a different configuration, use
 * [`wfc_world_state_slots_set`].
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
                                             const struct AdjacencyRule *adjacency_rules_ptr,
                                             uintptr_t adjacency_rules_len,
                                             uint16_t world_x,
                                             uint16_t world_y,
                                             uint16_t world_z,
                                             Entropy entropy);

/**
 * Creates an instance of Wave Function Collapse world state as a copy of
 * existing world state.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_world_state_handle_ptr` will be written to. It must be non-null and
 *   aligned.
 *
 * - `source_wfc_world_state_handle` must be a valid handle created via
 *   [`wfc_world_state_init`] that returned [`WfcWorldStateInitResult::Ok`] or
 *   [`wfc_world_state_init_from`] and not yet freed via
 *   [`wfc_world_state_free`],
 */
void wfc_world_state_init_from(WfcWorldStateHandle *wfc_world_state_handle_ptr,
                               WfcWorldStateHandle source_wfc_world_state_handle);

/**
 * Copies data between two instances of Wave Function Collapse world state.
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
void wfc_world_state_clone_from(WfcWorldStateHandle destination_wfc_world_state_handle,
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
 *   [`wfc_world_state_free`],
 */
void wfc_world_state_free(WfcWorldStateHandle wfc_world_state_handle);

/**
 * Writes Wave Function Collapse slots from `slots_ptr` and `slots_len` into
 * the provided handle.
 *
 * Slots are stored in sparse bit vectors where each bit encodes a module
 * present at that slot, e.g. a slot with 0th and 2nd bits set will contain
 * modules with ids 0 and 2.
 *
 * Currently does not validate against setting bits higher than the module
 * count, but it is a usage error to do so.
 *
 * The bit vectors of state are stored in a three dimensional array (compacted
 * in a one dimensional array). To get to a slot state on position `[x, y, z]`,
 * first slice by Z, then Y, then X. E.g. for dimensions 2*2*2, the slots would
 * be in the following order:
 *
 * ```text
 * [0, 0, 0]
 * [1, 0, 0]
 * [0, 1, 0]
 * [1, 1, 0]
 * [0, 0, 1]
 * [1, 0, 1]
 * [0, 1, 1]
 * [1, 1, 1]
 * ```
 *
 * If this function returns
 * [`WfcWorldStateSlotsSetResult::ErrWorldContradictory`], the provided handle
 * becomes invalid. It will become valid once again when passed to this
 * function and [`WfcWorldStateSlotsSetResult::Ok`] or
 * [`WfcWorldStateSlotsSetResult::OkWorldNotCanonical`] is returned.
 *
 * If the modules in the provided slots could still be collapsed according to
 * the current rule set, the world is not canonical. This function fixes that
 * and returns [`WfcWorldStateSlotsSetResult::OkWorldNotCanonical`] as a
 * warning.
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
 * - `slots_ptr` and `slots_len` are used to construct a slice. See
 *   [`std::slice::from_raw_parts`].
 */
WfcWorldStateSlotsSetResult wfc_world_state_slots_set(WfcWorldStateHandle wfc_world_state_handle,
                                                      const uint64_t (*slots_ptr)[4],
                                                      uintptr_t slots_len);

/**
 * Gets the current world status without making an observation.
 */
WfcObserveResult wfc_world_status(WfcWorldStateHandle wfc_world_state_handle);

/**
 * Reads slots from the provided handle into `slots_ptr` and `slots_len`.
 *
 * State is stored in sparse bit vectors where each bit encodes a module
 * present at that slot, e.g. a slot with 0th and 2nd bits set will contain
 * modules with ids 0 and 2.
 *
 * The bit vectors of state are stored in a three dimensional array (compacted
 * in a one dimensional array). To get to a slot state on position `[x, y, z]`,
 * first slice by Z, then Y, then X. E.g. for dimensions 2*2*2, the slots would
 * be in the following order:
 *
 * ```text
 * [0, 0, 0]
 * [1, 0, 0]
 * [0, 1, 0]
 * [1, 1, 0]
 * [0, 0, 1]
 * [1, 0, 1]
 * [0, 1, 1]
 * [1, 1, 1]
 * ```
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
 * - `slots_ptr` and `slots_len` are used to construct a mutable slice. See
 *   [`std::slice::from_raw_parts_mut`].
 */
void wfc_world_state_slots_get(WfcWorldStateHandle wfc_world_state_handle,
                               uint64_t (*slots_ptr)[4],
                               uintptr_t slots_len);

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
 *   [`wfc_rng_state_init`] and not yet freed via [`wfc_rng_state_free`].
 */
WfcObserveResult wfc_observe(WfcWorldStateHandle wfc_world_state_handle,
                             WfcRngStateHandle wfc_rng_state_handle,
                             uint32_t max_observations,
                             uint32_t *spent_observations);
