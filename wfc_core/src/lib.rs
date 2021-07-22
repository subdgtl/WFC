mod bitvec;
mod constbitvec;
mod convert;

use std::cmp::Ordering;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::fmt;

use arrayvec::ArrayVec;
use fxhash::FxBuildHasher;
use oorandom::Rand64;

use crate::bitvec::BitVec;
use crate::constbitvec::ConstBitVec;
use crate::convert::cast_u8;

pub const MAX_MODULE_COUNT: u32 = BitVec::MAX_LEN as u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AdjacencyKind {
    X,
    Y,
    Z,
}

impl From<Direction> for AdjacencyKind {
    fn from(direction: Direction) -> Self {
        match direction {
            Direction::Left => AdjacencyKind::X,
            Direction::Right => AdjacencyKind::X,
            Direction::Front => AdjacencyKind::Y,
            Direction::Back => AdjacencyKind::Y,
            Direction::Down => AdjacencyKind::Z,
            Direction::Up => AdjacencyKind::Z,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Adjacency {
    pub kind: AdjacencyKind,
    pub module_low: u16,
    pub module_high: u16,
}

/// Solver ([`World`]) features that have to be explicitly enabled when
/// initializing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Features(u32);

impl Features {
    /// Slot entropy calculation utilizes weights. Will allocate memory for
    /// weights if enabled.
    pub const WEIGHTED_ENTROPY: Self = Self(0x01);

    /// Module selection during observation performs weighted random. Will
    /// allocate memory for weights if enabled.
    pub const WEIGHTED_OBSERVATION: Self = Self(0x02);

    pub const fn into_bits(self) -> u32 {
        self.0
    }

    pub const fn from_bits_truncate(raw: u32) -> Self {
        const ALL: u32 = 0x01 | 0x02;
        Self(raw & ALL)
    }

    pub const fn empty() -> Self {
        Self(0)
    }

    pub fn contains_any_weighted(&self) -> bool {
        const ALL_WEIGHTED: u32 = 0x01 | 0x02;
        self.0 & ALL_WEIGHTED > 0
    }

    pub fn has_weighted_entropy(&self) -> bool {
        self.0 & Self::WEIGHTED_ENTROPY.0 > 0
    }

    pub fn has_weighted_observation(&self) -> bool {
        self.0 & Self::WEIGHTED_OBSERVATION.0 > 0
    }
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
pub enum WorldStatus {
    Deterministic,
    Nondeterministic,
    Contradiction,
}

impl fmt::Display for WorldStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Nondeterministic => write!(f, "non-deterministic"),
            Self::Deterministic => write!(f, "deterministic"),
            Self::Contradiction => write!(f, "contradiction"),
        }
    }
}

// Note: We could have used Rand32, but world position is up to 48 bits, so we
// use 64 bit RNG.
pub struct Rng(Rand64);

impl Rng {
    pub fn new(seed: u128) -> Self {
        Self(Rand64::new(seed))
    }
}

#[derive(Clone)]
enum Slots {
    // One block, 64 bits, 6-bit addressing, 58 data bits
    S1(Vec<ConstBitVec<1>>),
    // Two blocks - 128 bits, 7-bit addressing, 121 data bits
    S2(Vec<ConstBitVec<2>>),
    // Four blocks - 256 bits, 8-bit addressing, 248 data bits
    S4(Vec<ConstBitVec<4>>),
    // Eight blocks - 512 bits, 9-bit addressing, 503 data bits
    S8(Vec<ConstBitVec<8>>),
    // Sixteen blocks - 1024 bits, 10-bit addressing, 1014 data bits
    S16(Vec<ConstBitVec<8>>),
}

impl Slots {
    pub const MAX_MODULE_COUNT: usize = 1014;

    pub fn new(slot_count: usize, module_count: usize) -> Option<Slots> {
        match module_count {
            0..=58 => Some(Self::S1(vec![ConstBitVec::zeros(); slot_count])),
            59..=121 => Some(Self::S2(vec![ConstBitVec::zeros(); slot_count])),
            122..=248 => Some(Self::S4(vec![ConstBitVec::zeros(); slot_count])),
            249..=503 => Some(Self::S8(vec![ConstBitVec::zeros(); slot_count])),
            504..=1014 => Some(Self::S16(vec![ConstBitVec::zeros(); slot_count])),
            _ => None,
        }
    }

    pub fn contains(&self, slot_index: usize, module_index: usize) -> bool {
        match self {
            Self::S1(slots) => slots[slot_index].contains(module_index),
            Self::S2(slots) => slots[slot_index].contains(module_index),
            Self::S4(slots) => slots[slot_index].contains(module_index),
            Self::S8(slots) => slots[slot_index].contains(module_index),
            Self::S16(slots) => slots[slot_index].contains(module_index),
        }
    }

    pub fn add(&mut self, slot_index: usize, module_index: usize) -> bool {
        match self {
            Self::S1(slots) => slots[slot_index].add(module_index),
            Self::S2(slots) => slots[slot_index].add(module_index),
            Self::S4(slots) => slots[slot_index].add(module_index),
            Self::S8(slots) => slots[slot_index].add(module_index),
            Self::S16(slots) => slots[slot_index].add(module_index),
        }
    }

    pub fn remove(&mut self, slot_index: usize, module_index: usize) -> bool {
        match self {
            Self::S1(slots) => slots[slot_index].remove(module_index),
            Self::S2(slots) => slots[slot_index].remove(module_index),
            Self::S4(slots) => slots[slot_index].remove(module_index),
            Self::S8(slots) => slots[slot_index].remove(module_index),
            Self::S16(slots) => slots[slot_index].remove(module_index),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::S1(slots) => slots.len(),
            Self::S2(slots) => slots.len(),
            Self::S4(slots) => slots.len(),
            Self::S8(slots) => slots.len(),
            Self::S16(slots) => slots.len(),
        }
    }
}

#[derive(Clone)]
pub struct World {
    dims: [u16; 3],
    adjacencies: Vec<Adjacency>,
    features: Features,

    slots: Slots,
    slot_module_weights: Option<Vec<f32>>,
    module_count: usize,

    /// Working memory for picking the nondeterministic slot with smallest
    /// entropy. Pre-allocated to maximum capacity.
    min_entropy_slots: Vec<usize>,
}

impl World {
    pub fn new(
        dims: [u16; 3],
        adjacencies: Vec<Adjacency>,
        features: Features,
    ) -> Result<Self, WorldNewError> {
        if dims[0] == 0 || dims[1] == 0 || dims[2] == 0 {
            return Err(WorldNewError::DimensionsZero);
        }

        assert!(!adjacencies.is_empty());

        let mut modules = BitVec::zeros();
        let mut module_count: usize = 0;
        let mut module_max: usize = 0;

        for adjacency in &adjacencies {
            let low = usize::from(adjacency.module_low);
            let high = usize::from(adjacency.module_high);

            if low > module_max {
                module_max = low;
            }
            if high > module_max {
                module_max = high;
            }
        }

        if module_max >= Slots::MAX_MODULE_COUNT {
            return Err(WorldNewError::TooManyModules);
        }

        let slot_count = usize::from(dims[0]) * usize::from(dims[1]) * usize::from(dims[2]);
        let mut slots =
            Slots::new(slot_count, module_count).ok_or(WorldNewError::TooManyModules)?;

        for adjacency in &adjacencies {
            if slots.add(0, usize::from(adjacency.module_low)) {
                module_count += 1;
            }
            if slots.add(0, usize::from(adjacency.module_high)) {
                module_count += 1;
            }
        }

        if module_count != usize::from(module_max) + 1 {
            return Err(WorldNewError::AdjacenciesHaveGaps);
        }

        // XXX: Spread data from first slot to rest

        let slot_module_weights = if features.contains_any_weighted() {
            Some(vec![1.0; slot_count * module_count])
        } else {
            None
        };

        Self {
            dims,
            adjacencies,
            features,

            slots,
            slot_module_weights,
            module_count,

            min_entropy_slots: Vec::with_capacity(slot_count),
        }
    }

    pub fn clone_from(&mut self, other: &World) {
        self.dims = other.dims;
        self.adjacencies.clone_from(&other.adjacencies);
        self.features = other.features;

        self.slots.clone_from(&other.slots);
        self.slot_module_weights
            .clone_from(&other.slot_module_weights);

        self.module_count = other.module_count;
    }

    pub fn dims(&self) -> [u16; 3] {
        self.dims
    }

    pub fn slot_count(&self) -> usize {
        self.slots.len()
    }

    pub fn module_count(&self) -> usize {
        self.module_count
    }

    pub fn slot_module(&self, pos: [u16; 3], module: u16) -> bool {
        let index = position_to_index(self.slots.len(), self.dims, pos);
        self.slots.contains(index, usize::from(module))
    }

    pub fn set_slot_module(&mut self, pos: [u16; 3], module: u16, value: bool) {
        let index = position_to_index(self.slots.len(), self.dims, pos);
        if value {
            self.slots.add(index, usize::from(module));
        } else {
            self.slots.remove(index, usize::from(module));
        }
    }

    pub fn set_slot_modules(&mut self, pos: [u16; 3], value: bool) {
        let index = position_to_index(self.slots.len(), self.dims, pos);

        // XXX: @Speed Cache the array access?
        for module in 0..self.module_count {
            if value {
                self.slots.add(index, module);
            } else {
                self.slots.remove(index, module);
            }
        }
    }

    pub fn slot_modules_iter(&self, pos: [u16; 3]) -> impl Iterator<Item = u8> + '_ {
        let index = position_to_index(self.slots.len(), self.dims, pos);
        let slot = &self.slots[index];

        slot.iter()
    }

    /// Sets weights for a module.
    ///
    /// Computations running on the weights require them to be normal, positive
    /// floats (not zero, not infinite, not NaN, not subnormal and sign
    /// positive).
    ///
    /// # Panics
    ///
    /// Panics if any of the weights is not a normal ([`f32::is_normal`]),
    /// positive ([`f32::is_sign_positive`]) number.
    pub fn set_slot_module_weights(&mut self, pos: [u16; 3], weights: &[f32]) {
        assert_eq!(weights.len(), self.module_count);
        for weight in weights {
            assert!(weight.is_normal() && weight.is_sign_positive());
        }

        if let Some(slot_module_weights) = &mut self.slot_module_weights {
            let index_base =
                self.module_count * position_to_index(self.slots.len(), self.dims, pos);

            let module_weights =
                &mut slot_module_weights[index_base..index_base + self.module_count];
            module_weights.copy_from_slice(weights);
        }
    }

    pub fn ensure_constraints(&mut self) -> (bool, WorldStatus) {
        let mut world_changed = false;

        for i in 0..self.slots.len() {
            let (changed, contradiction) = self.propagate_constraints(i);

            world_changed |= changed;

            // Do not continue for worlds where there is nothing more to do,
            // otherwise we would trigger asserts.

            // For some contradictions, we know immediately ...
            if contradiction {
                return (world_changed, WorldStatus::Contradiction);
            }

            // ... but we still need to do a comprehensive check to find
            // contradictions elsewhere. We only return immediately for
            // contradictions. Deterministic or Nondeterministic don't mean
            // anything yet, because we may still remove more modules, possibly
            // transitioning Nondeterministic to Deterministic or Deterministic
            // to Contractiction.
            match self.world_status() {
                WorldStatus::Nondeterministic => (),
                WorldStatus::Deterministic => (),
                WorldStatus::Contradiction => {
                    return (world_changed, WorldStatus::Contradiction);
                }
            }
        }

        (world_changed, self.world_status())
    }

    pub fn observe(&mut self, rng: &mut Rng) -> (bool, WorldStatus) {
        let mut min_entropy = f32::INFINITY;
        self.min_entropy_slots.clear();

        for (i, slot) in self.slots.iter().enumerate() {
            // We can collapse anything with slot_len >= 2. If slot_len == 1,
            // the slot is already collapsed. If slot_len == 0, we hit a
            // contradiction and can bail out early.
            let slot_len = slot.len();
            if slot_len == 0 {
                return (false, WorldStatus::Contradiction);
            }
            if slot_len == 1 {
                continue;
            }

            let entropy = if self.features.has_weighted_entropy() {
                let slot_module_weights = self.slot_module_weights.as_ref().unwrap();
                let mut sum_weights = 0.0;
                let mut sum_weight_log_weights = 0.0;
                for module in slot {
                    let weight_index = self.module_count * i + usize::from(module);
                    let weight = slot_module_weights[weight_index];
                    sum_weights += weight;
                    sum_weight_log_weights += weight * weight.ln();
                }

                assert!(sum_weights >= 0.0);

                let entropy = sum_weights.ln() - sum_weight_log_weights / sum_weights;
                assert!(!entropy.is_nan());

                entropy
            } else {
                slot_len as f32
            };

            match entropy.partial_cmp(&min_entropy) {
                Some(Ordering::Less) => {
                    min_entropy = entropy;
                    self.min_entropy_slots.clear();
                    self.min_entropy_slots.push(i);
                }
                Some(Ordering::Equal) => {
                    self.min_entropy_slots.push(i);
                }
                Some(Ordering::Greater) => (),
                None => (),
            }
        }

        if self.min_entropy_slots.is_empty() {
            (false, WorldStatus::Deterministic)
        } else {
            let rand64 = &mut rng.0;

            let min_entropy_slot_index = choose_slot(rand64, &self.min_entropy_slots);
            let min_entropy_slot = &mut self.slots[min_entropy_slot_index];

            // Pick a random module to materialize and remove other
            // possibilities from the slot.
            let chosen_module = if self.features.has_weighted_observation() {
                let slot_module_weights = self.slot_module_weights.as_ref().unwrap();
                let index_base = self.module_count * min_entropy_slot_index;
                let weights = &slot_module_weights[index_base..index_base + self.module_count];

                choose_module_weighted(rand64, &min_entropy_slot, weights)
            } else {
                choose_module(rand64, &min_entropy_slot)
            };

            min_entropy_slot.clear();
            min_entropy_slot.add(chosen_module);

            let (changed, contradiction) = self.propagate_constraints(min_entropy_slot_index);
            if contradiction {
                (changed, WorldStatus::Contradiction)
            } else {
                let status = self.world_status();
                (changed, status)
            }
        }
    }

    pub fn world_status(&self) -> WorldStatus {
        let mut lt1 = false;
        let mut gt1 = false;
        for slot in &self.slots {
            match slot.len().cmp(&1) {
                Ordering::Less => {
                    lt1 = true;
                }
                Ordering::Equal => (),
                Ordering::Greater => {
                    gt1 = true;
                }
            }
        }

        match (lt1, gt1) {
            (false, false) => WorldStatus::Deterministic,
            (false, true) => WorldStatus::Nondeterministic,
            (true, _) => WorldStatus::Contradiction,
        }
    }

    fn propagate_constraints(&mut self, slot_index: usize) -> (bool, bool) {
        let slots_len = self.slots.len();
        let [dim_x, dim_y, dim_z] = self.dims;

        let mut visited = HashSet::with_hasher(FxBuildHasher::default());
        visited.insert(slot_index);

        let mut stack = vec![StackEntry {
            // Do not start in the init state here, b/c we assume the change
            // already happened and we only propagate it.
            search_state: SearchState::SearchLeft,
            // Invalid, but won't be looked at, because we skip the init state
            search_direction: Direction::Right,
            slot_index,
            // Invalid, but won't be looked at, because we skip the init state
            slot_index_prev: slot_index,
        }];

        let mut changed = false;
        let mut contradiction = false;
        while !stack.is_empty() && !contradiction {
            let s = stack.last_mut().unwrap();

            match s.search_state {
                SearchState::Init => {
                    visited.insert(s.slot_index);

                    let slot = &self.slots[s.slot_index];
                    let slot_prev = &self.slots[s.slot_index_prev];

                    let mut new_slot = BitVec::zeros();
                    for adj in self
                        .adjacencies
                        .iter()
                        .filter(|adj| adj.kind == AdjacencyKind::from(s.search_direction))
                        .filter(|adj| {
                            if s.search_direction.is_positive() {
                                slot_prev.contains(adj.module_low) && slot.contains(adj.module_high)
                            } else {
                                slot_prev.contains(adj.module_high) && slot.contains(adj.module_low)
                            }
                        })
                    {
                        if s.search_direction.is_positive() {
                            new_slot.add(adj.module_high);
                        } else {
                            new_slot.add(adj.module_low);
                        }
                    }

                    let slot_len = slot.len();
                    let new_slot_len = new_slot.len();
                    assert!(slot_len > 0);
                    assert!(slot_len >= new_slot_len);

                    self.slots[s.slot_index] = new_slot;

                    if new_slot_len == 0 {
                        changed = true;
                        contradiction = true;

                        // We removed everything, stop the current observation iteration
                        s.search_state = SearchState::Done;
                    } else if slot_len == new_slot_len {
                        // We didn't remove anything, stop propagating this branch
                        s.search_state = SearchState::Done;
                    } else {
                        assert!(slot_len > new_slot_len);

                        changed = true;

                        // We removed something, propagate further
                        s.search_state = SearchState::SearchLeft;
                    }
                }
                SearchState::SearchLeft => {
                    let slot_index = s.slot_index;
                    let pos = index_to_position(slots_len, self.dims, slot_index);
                    let pos_next = [prev_position_saturating(pos[0], dim_x), pos[1], pos[2]];

                    s.search_state = SearchState::SearchRight;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    let pos_next = [next_position_saturating(pos[0], dim_x), pos[1], pos[2]];

                    s.search_state = SearchState::SearchFront;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    let pos_next = [pos[0], prev_position_saturating(pos[1], dim_y), pos[2]];

                    s.search_state = SearchState::SearchBack;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    let pos_next = [pos[0], next_position_saturating(pos[1], dim_y), pos[2]];

                    s.search_state = SearchState::SearchDown;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    let pos_next = [pos[0], pos[1], prev_position_saturating(pos[2], dim_z)];

                    s.search_state = SearchState::SearchUp;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    let pos_next = [pos[0], pos[1], next_position_saturating(pos[2], dim_z)];

                    s.search_state = SearchState::Done;

                    if pos != pos_next {
                        let slot_index_next = position_to_index(slots_len, self.dims, pos_next);
                        if !visited.contains(&slot_index_next) {
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
                    visited.remove(&s.slot_index);
                    stack.pop();

                    // We are done, do not advance search state any further
                }
            }
        }

        (changed, contradiction)
    }
}

fn choose_slot(rng: &mut Rand64, slots: &[usize]) -> usize {
    assert!(!slots.is_empty());

    let rand_num = rng.rand_u64() as usize;
    let index = rand_num % slots.len();
    slots[index]
}

fn choose_module(rng: &mut Rand64, slot: &BitVec) -> u8 {
    assert!(slot.len() > 0);

    let rand_num = rng.rand_u64() as usize;
    let index = rand_num % slot.len();
    slot.iter().nth(index).unwrap()
}

fn choose_module_weighted(rng: &mut Rand64, slot: &BitVec, weights: &[f32]) -> u8 {
    const MAX_LEN: usize = BitVec::MAX_LEN as usize;

    assert!(slot.len() > 0);
    assert!(weights.len() <= MAX_LEN);

    // The following search over accumulated weights is ok and the unwraps
    // should never panic. This is because when we compute cummulative weights,
    // we don't increment the value unless there is a module present in the
    // slot, and our assert at the top tells us there is at least one module
    // present. This means we should eventually find a module for which our
    // random number selects the weight.
    //
    // Additionally, because absent modules don't increment the cummulative
    // weight, we know that they are "shadowed" by previously visited present
    // modules, and therefore can't be selected. However, if the first module is
    // absent *and* the RNG rolls zero, we could incorrectly select it. For this
    // reason, we track whether we have already visited a present module.

    let mut cummulative_weight: f32 = 0.0;
    let mut cummulative_weights: ArrayVec<f32, MAX_LEN> = ArrayVec::new();

    for (i, weight) in weights.iter().enumerate() {
        if slot.contains(cast_u8(i)) {
            cummulative_weight += weight;
        }

        let value = if cummulative_weight == 0.0 {
            -1.0
        } else {
            cummulative_weight
        };

        cummulative_weights.push(value);
    }

    let rand_num = rng.rand_float() as f32 * cummulative_weight;
    let index = cummulative_weights
        .iter()
        .enumerate()
        .find_map(|(i, cummulative_weight)| {
            if rand_num <= *cummulative_weight {
                Some(i)
            } else {
                None
            }
        })
        .unwrap();

    cast_u8(index)
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
    assert!(dim > 0);
    pos.saturating_sub(1)
}

fn next_position_saturating(pos: u16, dim: u16) -> u16 {
    assert!(dim > 0);
    pos.saturating_add(1).min(dim - 1)
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
