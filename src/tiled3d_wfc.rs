use std::collections::HashSet;
use std::convert::TryFrom;
use std::fmt;
use std::num::NonZeroU32;

// FIXME: hibitset::Bitset is quite a lot of overhead if we don't utilize its
// dynamic memory. We could make a `SmallBitSet` which doesn't dynamically
// allocate for small amount of elements, if we find out our number of modules
// is usually below some reasonable number, like 256 or 512, as hibitset::BitSet
// is already quite large: 640 bits (with 64bit usize) = usize + 3 * 3 * usize.
use hibitset::BitSet;
use tinyvec::TinyVec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tiled3dAdjacencyKind {
    X,
    Y,
    Z,
}

impl From<Direction> for Tiled3dAdjacencyKind {
    fn from(direction: Direction) -> Self {
        match direction {
            Direction::Left => Tiled3dAdjacencyKind::X,
            Direction::Right => Tiled3dAdjacencyKind::X,
            Direction::Front => Tiled3dAdjacencyKind::Y,
            Direction::Back => Tiled3dAdjacencyKind::Y,
            Direction::Down => Tiled3dAdjacencyKind::Z,
            Direction::Up => Tiled3dAdjacencyKind::Z,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tiled3dAdjacency {
    pub kind: Tiled3dAdjacencyKind,
    pub module_low: NonZeroU32,
    pub module_high: NonZeroU32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackEntry {
    search_state: SearchState,
    search_direction: Direction,
    slot_index: usize,
    slot_index_prev: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SearchState {
    Init,
    SearchLeft,
    SearchRight,
    SearchFront,
    SearchBack,
    SearchDown,
    SearchUp,
    Done,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
    Front,
    Back,
    Down,
    Up,
}

impl Direction {
    fn is_positive(&self) -> bool {
        match self {
            Self::Left => false,
            Self::Right => true,
            Self::Front => false,
            Self::Back => true,
            Self::Down => false,
            Self::Up => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tiled3dObserveResult {
    Nondeterministic,
    Deterministic,
    Contradiction,
}

impl fmt::Display for Tiled3dObserveResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nondeterministic => write!(f, "non-deterministic"),
            Self::Deterministic => write!(f, "deterministic"),
            Self::Contradiction => write!(f, "contradiction"),
        }
    }
}

#[derive(Debug)]
pub struct Tiled3dWorld {
    dims: [u16; 3],
    adjacencies: Vec<Tiled3dAdjacency>,
    slots: Vec<BitSet>,
    module_count: u32,
    wrapping: bool,
}

impl Tiled3dWorld {
    pub fn new(dims: [u16; 3], adjacencies: Vec<Tiled3dAdjacency>, wrapping: bool) -> Self {
        assert!(dims[0] > 0);
        assert!(dims[1] > 0);
        assert!(dims[2] > 0);

        assert!(!adjacencies.is_empty());

        let mut modules = BitSet::new();
        let mut module_count = 0;
        let mut module_max = 0;

        for adjacency in &adjacencies {
            if !modules.add(adjacency.module_low.get()) {
                module_count += 1;
            }
            if !modules.add(adjacency.module_high.get()) {
                module_count += 1;
            }

            if adjacency.module_low.get() > module_max {
                module_max = adjacency.module_low.get();
            }
            if adjacency.module_high.get() > module_max {
                module_max = adjacency.module_high.get();
            }
        }

        // This assert shouldn't be possible to break if the importer works
        // correctly. The importer should intern names to sequentially
        // allocated numbers AND starts the id sequence at 1 (0 is reserved for
        // the void module).
        assert!(
            module_count == module_max,
            "No gaps in module indices allowed",
        );

        assert_eq!(
            modules.contains(0),
            false,
            "The 0-th module is reserved and cannot be set implicitly",
        );

        let slot_count = usize::from(dims[0]) * usize::from(dims[1]) * usize::from(dims[2]);
        let mut slots = Vec::with_capacity(slot_count);

        for _ in 0..slot_count {
            slots.push(modules.clone());
        }

        Self {
            dims,
            adjacencies,
            slots,
            module_count,
            wrapping,
        }
    }

    pub fn export_slots(&self) -> Vec<TinyVec<[u32; 4]>> {
        use hibitset::BitSetLike as _;
        self.slots
            .iter()
            .map(|bitset| bitset.iter().collect())
            .collect()
    }

    pub fn reset(&mut self) {
        for slot in &mut self.slots {
            for i in 1..self.module_count {
                slot.add(i);
            }

            assert_eq!(
                slot.contains(0),
                false,
                "The 0-th module is reserved and cannot be set implicitly",
            );
        }
    }

    pub fn reset_to_initial_state(&mut self, initial_state: &[TinyVec<[u32; 4]>]) {
        assert_eq!(self.slots.len(), initial_state.len());

        for (i, slot) in self.slots.iter_mut().enumerate() {
            let initial_state_is_void = initial_state[i].contains(&0);
            if initial_state_is_void {
                assert_eq!(
                    initial_state[i].len(),
                    1,
                    "Slots initialized with the void module must not contain other modules",
                );
            }

            slot.clear();

            for module in &initial_state[i] {
                slot.add(*module);
            }
        }
    }

    pub fn observe<R: rand::Rng>(&mut self, rng: &mut R) -> Tiled3dObserveResult {
        let mut min_entropy_slot_index_and_value: Option<(usize, usize)> = None;
        for (i, slot) in self.slots.iter().enumerate() {
            let entropy = count_ones(slot);
            // We can collapse anything with entropy >= 2. If entropy == 1, the
            // slot is already collapsed. If entropy is 0, we hit a
            // contradiction and can bail out early.
            if entropy == 0 {
                return Tiled3dObserveResult::Contradiction;
            }

            if entropy >= 2 {
                match min_entropy_slot_index_and_value {
                    Some((_, min_entropy)) => {
                        if entropy < min_entropy {
                            min_entropy_slot_index_and_value = Some((i, entropy));
                        }
                    }
                    None => {
                        min_entropy_slot_index_and_value = Some((i, entropy));
                    }
                }
            }
        }

        if let Some((min_entropy_slot_index, _)) = min_entropy_slot_index_and_value {
            let slots_len = self.slots.len();
            let min_entropy_slot = &mut self.slots[min_entropy_slot_index];

            // Pick a random module to materialize and remove other
            // possibilities from the slot.
            let chosen_module = choose_random(min_entropy_slot, rng);
            log::debug!(
                "PICK  {:>3?} {}",
                index_to_position(slots_len, self.dims, min_entropy_slot_index),
                chosen_module,
            );

            min_entropy_slot.clear();
            min_entropy_slot.add(chosen_module);

            let [dim_x, dim_y, dim_z] = self.dims;

            let mut visited = HashSet::new();
            visited.insert(min_entropy_slot_index);

            let mut stack = vec![StackEntry {
                // Do not start in the init state here, because we already
                // collapsed this slot
                search_state: SearchState::SearchLeft,
                // Invalid, but won't be looked at, because we skip the init state
                search_direction: Direction::Right,
                slot_index: min_entropy_slot_index,
                // Invalid, but won't be looked at, because we skip the init state
                slot_index_prev: min_entropy_slot_index,
            }];

            let mut contradiction = false;
            while !stack.is_empty() && !contradiction {
                let s = stack.last_mut().unwrap();

                match s.search_state {
                    SearchState::Init => {
                        visited.insert(s.slot_index);

                        let slot = &self.slots[s.slot_index];
                        let slot_prev = &self.slots[s.slot_index_prev];

                        // FIXME: @Optimization This is only allocated to
                        // appease the borrowchecker. Can we not allocate the
                        // bit difference using slice::split_at_mut or similar?
                        let mut new_slot = BitSet::new();

                        for adj in self
                            .adjacencies
                            .iter()
                            .filter(|adj| {
                                adj.kind == Tiled3dAdjacencyKind::from(s.search_direction)
                            })
                            .filter(|adj| {
                                if s.search_direction.is_positive() {
                                    slot_prev.contains(adj.module_low.get())
                                        && slot.contains(adj.module_high.get())
                                } else {
                                    slot_prev.contains(adj.module_high.get())
                                        && slot.contains(adj.module_low.get())
                                }
                            })
                        {
                            if s.search_direction.is_positive() {
                                new_slot.add(adj.module_high.get());
                            } else {
                                new_slot.add(adj.module_low.get());
                            }
                        }

                        let slot_len = count_ones(slot);
                        let new_slot_len = count_ones(&new_slot);
                        debug_assert!(slot_len > 0);
                        debug_assert!(slot_len >= new_slot_len);

                        self.slots[s.slot_index] = new_slot;

                        log::debug!(
                            "INIT  {:>3?} {} -> {}",
                            index_to_position(slots_len, self.dims, s.slot_index),
                            slot_len,
                            new_slot_len,
                        );

                        if new_slot_len == 0 {
                            // We removed everything, stop the current observation iteration
                            contradiction = true;
                            s.search_state = SearchState::Done;
                        } else if slot_len == new_slot_len {
                            // We didn't remove anything, stop propagating this branch
                            s.search_state = SearchState::Done;
                        } else {
                            debug_assert!(slot_len > new_slot_len);
                            // We removed something, propagate further
                            s.search_state = SearchState::SearchLeft;
                        }
                    }
                    SearchState::SearchLeft => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [prev_position_wrapping(pos[0], dim_x), pos[1], pos[2]]
                        } else {
                            [prev_position_saturating(pos[0], dim_x), pos[1], pos[2]]
                        };

                        s.search_state = SearchState::SearchRight;

                        if pos != pos_next {
                            log::debug!("LEFT  {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Left,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::SearchRight => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [next_position_wrapping(pos[0], dim_x), pos[1], pos[2]]
                        } else {
                            [next_position_saturating(pos[0], dim_x), pos[1], pos[2]]
                        };

                        s.search_state = SearchState::SearchFront;

                        if pos != pos_next {
                            log::debug!("RIGHT {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Right,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::SearchFront => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [pos[0], prev_position_wrapping(pos[1], dim_y), pos[2]]
                        } else {
                            [pos[0], prev_position_saturating(pos[1], dim_y), pos[2]]
                        };

                        s.search_state = SearchState::SearchBack;

                        if pos != pos_next {
                            log::debug!("FRONT {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Front,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::SearchBack => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [pos[0], next_position_wrapping(pos[1], dim_y), pos[2]]
                        } else {
                            [pos[0], next_position_saturating(pos[1], dim_y), pos[2]]
                        };

                        s.search_state = SearchState::SearchDown;

                        if pos != pos_next {
                            log::debug!("BACK  {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Back,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::SearchDown => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [pos[0], pos[1], prev_position_wrapping(pos[2], dim_z)]
                        } else {
                            [pos[0], pos[1], prev_position_saturating(pos[2], dim_z)]
                        };

                        s.search_state = SearchState::SearchUp;

                        if pos != pos_next {
                            log::debug!("DOWN  {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Down,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::SearchUp => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(slots_len, self.dims, slot_index);
                        let pos_next = if self.wrapping {
                            [pos[0], pos[1], next_position_wrapping(pos[2], dim_z)]
                        } else {
                            [pos[0], pos[1], next_position_saturating(pos[2], dim_z)]
                        };

                        s.search_state = SearchState::Done;

                        if pos != pos_next {
                            log::debug!("UP    {:>3?} {:>3?}", pos, pos_next);

                            let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                            let slot_next = &self.slots[slot_index_next];

                            // Only propagate if we didn't visit yet AND the
                            // slot is not a void slot
                            if !visited.contains(&slot_index_next) && !slot_next.contains(0) {
                                stack.push(StackEntry {
                                    search_state: SearchState::Init,
                                    search_direction: Direction::Up,
                                    slot_index: slot_index_next,
                                    slot_index_prev: slot_index,
                                });
                            }
                        }
                    }
                    SearchState::Done => {
                        log::debug!(
                            "DONE  {:>3?}",
                            index_to_position(slots_len, self.dims, s.slot_index),
                        );
                        visited.remove(&s.slot_index);
                        stack.pop();

                        // We are done, do not advance search state any further
                    }
                }
            }

            if contradiction {
                Tiled3dObserveResult::Contradiction
            } else {
                // FIXME: This may be cleaner to check elsewhere?
                let mut collapsed = true;
                for slot in &self.slots {
                    if count_ones(slot) > 1 {
                        collapsed = false;
                    }
                }

                if collapsed {
                    Tiled3dObserveResult::Deterministic
                } else {
                    Tiled3dObserveResult::Nondeterministic
                }
            }
        } else {
            Tiled3dObserveResult::Deterministic
        }
    }
}

fn count_ones(bit_set: &BitSet) -> usize {
    use hibitset::BitSetLike as _;
    bit_set.iter().count()
}

fn choose_random<R: rand::Rng>(bit_set: &BitSet, rng: &mut R) -> u32 {
    use hibitset::BitSetLike as _;
    use rand::seq::IteratorRandom as _;
    bit_set.iter().choose(rng).unwrap()
}

pub fn position_to_index(len: usize, dims: [u16; 3], position: [u16; 3]) -> usize {
    let [x, y, z] = position;

    assert!(x < dims[0]);
    assert!(y < dims[1]);
    assert!(z < dims[2]);

    let dim_x = usize::from(dims[0]);
    let dim_y = usize::from(dims[1]);

    let slots_per_layer = dim_x * dim_y;
    let slots_per_row = dim_x;

    let index = usize::from(x) + usize::from(y) * slots_per_row + usize::from(z) * slots_per_layer;

    assert!(index < len);

    index
}

pub fn index_to_position(len: usize, dims: [u16; 3], index: usize) -> [u16; 3] {
    assert!(index < len);

    let dim_x = usize::from(dims[0]);
    let dim_y = usize::from(dims[1]);

    let slots_per_layer = dim_x * dim_y;
    let slots_per_row = dim_x;

    let x = u16::try_from(index % slots_per_layer % slots_per_row).unwrap();
    let y = u16::try_from(index % slots_per_layer / slots_per_row).unwrap();
    let z = u16::try_from(index / slots_per_layer).unwrap();

    assert!(x < dims[0]);
    assert!(y < dims[1]);
    assert!(z < dims[2]);

    [x, y, z]
}

fn prev_position_saturating(pos: u16, dim: u16) -> u16 {
    debug_assert!(dim > 0);
    pos.saturating_sub(1)
}

fn next_position_saturating(pos: u16, dim: u16) -> u16 {
    debug_assert!(dim > 0);
    pos.saturating_add(1).min(dim - 1)
}

fn prev_position_wrapping(pos: u16, dim: u16) -> u16 {
    debug_assert!(dim > 0);
    pos.wrapping_sub(1).min(dim - 1)
}

fn next_position_wrapping(pos: u16, dim: u16) -> u16 {
    debug_assert!(dim > 0);
    pos.wrapping_add(1) % dim
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_to_position_unit() {
        assert_eq!(index_to_position(1, [1, 1, 1], 0), [0, 0, 0]);
    }

    #[test]
    fn test_position_to_index_unit() {
        assert_eq!(position_to_index(1, [1, 1, 1], [0, 0, 0]), 0);
    }

    #[test]
    #[should_panic]
    fn test_index_to_position_unit_oob() {
        index_to_position(1, [1, 1, 1], 1);
    }

    #[test]
    #[should_panic]
    fn test_position_to_index_unit_oob() {
        position_to_index(1, [1, 1, 1], [0, 0, 1]);
    }
}
