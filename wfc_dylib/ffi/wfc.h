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

enum WfcInitResult {
  Ok = 0,
  TooManyModules = 1,
  WorldDimensionsZero = 2,
};
typedef uint32_t WfcInitResult;

typedef struct WfcState WfcState;

/**
 * An opaque handle to `WfcState`. Actually a pointer, but shhh!
 */
typedef WfcState *Wfc;

typedef struct {
  AdjacencyRuleKind kind;
  uint32_t module_low;
  uint32_t module_high;
} AdjacencyRule;

/**
 * Frees an instance of Wave Function Collapse state.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc` must be a valid handle created via `wfc_init` that returned
 *   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
 */
void wfc_free(Wfc wfc);

/**
 * Initializes Wave Function Collapse state with adjacency rules. The world
 * gets initialized with every module possible in every slot.
 *
 * To change the world state to a different configuration, use
 * `wfc_world_state_set`.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc_ptr` will be written to. It must be non-null and aligned.
 *
 * - `adjacency_rules_ptr` and `adjacency_rules_len` are used to construct a
 *    slice. See `slice::from_raw_parts`.
 */
WfcInitResult wfc_init(Wfc *wfc_ptr,
                       const AdjacencyRule *adjacency_rules_ptr,
                       uintptr_t adjacency_rules_len,
                       uint16_t world_x,
                       uint16_t world_y,
                       uint16_t world_z,
                       uint8_t rng_seed[16]);

/**
 * Runs observations on the world until a deterministic or contradictory result
 * is found.
 *
 * If contradictory result is found, the function tries again, up until
 * `max_attempts`.
 *
 * Returns the number of attemps it took to find a deterministic result or zero
 * if no deterministic result was found within `max_attempts` tries.
 *
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc` must be a valid handle created via `wfc_init` that returned
 *   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
 */
uint32_t wfc_observe(Wfc wfc, uint32_t max_attempts);

/**
 * Reads world state from the provided handle into `world_state_ptr` and
 * `world_state_len`.
 *
 * State is stored in sparse bit vectors where each bit encodes a module
 * present at that slot, e.g. a module with 0th and 2nd bits set will contain
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
 * - `wfc` must be a valid handle created via `wfc_init` that returned
 *   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
 *
 * - `world_state_ptr` and `world_state_len` are used to construct a mutable slice. See
 *   `slice::from_raw_parts_mut`.
 */
void wfc_world_state_get(Wfc wfc, uint64_t (*world_state_ptr)[8], uintptr_t world_state_len);

/**
 * Writes world state from `world_state_ptr` and `world_state_len` into the
 * provided handle.
 *
 * State is stored in sparse bit vectors where each bit encodes a module
 * present at that slot, e.g. a module with 0th and 2nd bits set will contain
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
 * # Safety
 *
 * Behavior is undefined if any of the following conditions are violated:
 *
 * - `wfc` must be a valid handle created via `wfc_init` that returned
 *   `WfcInitResult::Ok` and *not yet* freed via `wfc_free`.
 *
 * - `world_state_ptr` and `world_state_len` are used to construct a slice. See
 *   `slice::from_raw_parts`.
 */
void wfc_world_state_set(Wfc wfc, const uint64_t (*world_state_ptr)[8], uintptr_t world_state_len);
