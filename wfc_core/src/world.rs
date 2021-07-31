use std::cmp::Ordering;
use std::collections::HashSet;
use std::error;
use std::fmt;

use arrayvec::ArrayVec;
use fxhash::FxBuildHasher;
use oorandom::Rand64;

use crate::bitvec::BitVec;
use crate::convert::cast_u16;

use worldinner::WorldInner;
use worldinnerconst::WorldInnerConst;

pub const MAX_MODULE_COUNT: u16 = 1014;

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AdjacencyRuleKind {
    X = 0,
    Y = 1,
    Z = 2,
}

impl From<Direction> for AdjacencyRuleKind {
    fn from(direction: Direction) -> Self {
        match direction {
            Direction::Left => AdjacencyRuleKind::X,
            Direction::Right => AdjacencyRuleKind::X,
            Direction::Front => AdjacencyRuleKind::Y,
            Direction::Back => AdjacencyRuleKind::Y,
            Direction::Down => AdjacencyRuleKind::Z,
            Direction::Up => AdjacencyRuleKind::Z,
        }
    }
}

// TODO(yan): @Speed @Memory Because we now support a lot of modules, it is
// entirely possible to have > 10k, or even 100k rules (hard limit is around 3
// million). Does it make sense to make AdjacencyRule more compact for faster
// iteration?

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AdjacencyRule {
    pub kind: AdjacencyRuleKind,
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

    pub const ALL_WEIGHTED: Self = Self(Self::WEIGHTED_ENTROPY.0 | Self::WEIGHTED_OBSERVATION.0);
    pub const ALL: Self = Self::ALL_WEIGHTED;

    pub const fn bits(&self) -> u32 {
        self.0
    }

    pub const fn from_bits_truncate(bits: u32) -> Self {
        Self(bits & Self::ALL.0)
    }

    pub const fn empty() -> Self {
        Self(0)
    }

    pub fn contains_any_weighted(&self) -> bool {
        self.0 & Self::ALL_WEIGHTED.0 > 0
    }

    pub fn has_weighted_entropy(&self) -> bool {
        self.0 & Self::WEIGHTED_ENTROPY.0 > 0
    }

    pub fn has_weighted_observation(&self) -> bool {
        self.0 & Self::WEIGHTED_OBSERVATION.0 > 0
    }
}

pub fn position_to_index(dims: [u16; 3], position: [u16; 3]) -> usize {
    let slot_count = usize::from(dims[0]) * usize::from(dims[1]) * usize::from(dims[2]);

    let [x, y, z] = position;

    assert!(x < dims[0]);
    assert!(y < dims[1]);
    assert!(z < dims[2]);

    let dim_x = usize::from(dims[0]);
    let dim_y = usize::from(dims[1]);

    let slots_per_layer = dim_x * dim_y;
    let slots_per_row = dim_x;

    let index = usize::from(x) + usize::from(y) * slots_per_row + usize::from(z) * slots_per_layer;

    assert!(index < slot_count);

    index
}

pub fn index_to_position(dims: [u16; 3], index: usize) -> [u16; 3] {
    let slot_count = usize::from(dims[0]) * usize::from(dims[1]) * usize::from(dims[2]);

    assert!(index < slot_count);

    let dim_x = usize::from(dims[0]);
    let dim_y = usize::from(dims[1]);

    let slots_per_layer = dim_x * dim_y;
    let slots_per_row = dim_x;

    let x = cast_u16(index % slots_per_layer % slots_per_row);
    let y = cast_u16(index % slots_per_layer / slots_per_row);
    let z = cast_u16(index / slots_per_layer);

    assert!(x < dims[0]);
    assert!(y < dims[1]);
    assert!(z < dims[2]);

    [x, y, z]
}

// Note: We could have used Rand32, but world position is up to 48 bits, so we
// use 64 bit RNG.
pub struct Rng(Rand64);

impl Rng {
    pub fn new(seed: u128) -> Self {
        Self(Rand64::new(seed))
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WorldNewError {
    ModuleCountTooHigh,
    WorldDimensionsZero,
    RulesEmpty,
    RulesHaveGaps,
}

impl fmt::Display for WorldNewError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ModuleCountTooHigh => write!(f, "Module count too high"),
            Self::WorldDimensionsZero => write!(f, "World dimensions zero"),
            Self::RulesEmpty => write!(f, "Rules empty"),
            Self::RulesHaveGaps => write!(f, "Rules have gaps"),
        }
    }
}

impl error::Error for WorldNewError {}

#[derive(Clone)]
pub struct World {
    inner: WorldInner,
}

impl World {
    pub fn new(
        dims: [u16; 3],
        adjacency_rules: Vec<AdjacencyRule>,
        features: Features,
    ) -> Result<Self, WorldNewError> {
        if dims[0] == 0 || dims[1] == 0 || dims[2] == 0 {
            return Err(WorldNewError::WorldDimensionsZero);
        }

        if adjacency_rules.is_empty() {
            return Err(WorldNewError::RulesEmpty);
        }

        assert!(!adjacency_rules.is_empty());

        let mut module_count = 0;
        let mut module_max = 0;

        // TODO(yan): @Cleanup We have a max-sized slot here just for
        // validation. Volatile.
        let mut slot: BitVec<16> = BitVec::zeros();

        for adjacency in &adjacency_rules {
            if adjacency.module_low > module_max {
                module_max = adjacency.module_low;
            }
            if adjacency.module_high > module_max {
                module_max = adjacency.module_high;
            }

            if slot.add(adjacency.module_low) {
                module_count += 1;
            }
            if slot.add(adjacency.module_high) {
                module_count += 1;
            }
        }

        if module_count != module_max + 1 {
            return Err(WorldNewError::RulesHaveGaps);
        }

        let block_count = find_block_count_for_module_count(module_count)
            .ok_or(WorldNewError::ModuleCountTooHigh)?;

        let inner = match block_count {
            1 => WorldInner::Size1(WorldInnerConst::new(dims, adjacency_rules, features)),
            2 => WorldInner::Size2(WorldInnerConst::new(dims, adjacency_rules, features)),
            4 => WorldInner::Size4(WorldInnerConst::new(dims, adjacency_rules, features)),
            8 => WorldInner::Size8(WorldInnerConst::new(dims, adjacency_rules, features)),
            16 => WorldInner::Size16(WorldInnerConst::new(dims, adjacency_rules, features)),
            _ => unreachable!(),
        };

        Ok(Self { inner })
    }

    pub fn clone_compatible(&self, other: &Self) -> bool {
        self.inner.clone_compatible(&other.inner)
    }

    pub fn clone_from(&mut self, other: &Self) {
        self.inner.clone_from(&other.inner);
    }

    pub fn dims(&self) -> [u16; 3] {
        self.inner.dims()
    }

    pub fn module_count(&self) -> u16 {
        self.inner.module_count()
    }

    pub fn slots_modified(&self) -> bool {
        self.inner.slots_modified()
    }

    pub fn slot_count(&self) -> usize {
        self.inner.slot_count()
    }

    pub fn slot_module(&self, pos: [u16; 3], module: u16) -> bool {
        self.inner.slot_module(pos, module)
    }

    pub fn slot_module_count(&self, pos: [u16; 3]) -> usize {
        self.inner.slot_module_count(pos)
    }

    pub fn set_slot_module(&mut self, pos: [u16; 3], module: u16, value: bool) {
        self.inner.set_slot_module(pos, module, value);
    }

    pub fn set_slot_modules(&mut self, pos: [u16; 3], value: bool) {
        self.inner.set_slot_modules(pos, value);
    }

    /// Sets a weight for a module.
    ///
    /// Computations running on the weights require them to be normal, positive
    /// floats (not zero, not infinite, not NaN, not subnormal and sign
    /// positive).
    ///
    /// # Panics
    ///
    /// Panics if any of the weights is not a normal ([`f32::is_normal`]),
    /// positive ([`f32::is_sign_positive`]) number.
    pub fn set_slot_module_weight(&mut self, pos: [u16; 3], module: u16, weight: f32) {
        self.inner.set_slot_module_weight(pos, module, weight);
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
        self.inner.set_slot_module_weights(pos, weights);
    }

    pub fn canonicalize(&mut self) -> (bool, WorldStatus) {
        self.inner.canonicalize()
    }

    /// Runs one observation and propagates constraints.
    ///
    /// # Panics
    ///
    /// Panics if any slots are in the modified state. Use
    /// [`Self::canonicalize`] to make them canonical.
    pub fn observe(&mut self, rng: &mut Rng) -> (bool, WorldStatus) {
        self.inner.observe(rng)
    }

    /// Computes the world status.
    ///
    /// # Warning
    ///
    /// If any slots are in modified state (i.e. the world is possibly not
    /// canonical), the status might be incorrect.
    pub fn world_status(&self) -> WorldStatus {
        self.inner.world_status()
    }
}

// TODO(yan): @Cleanup Algorithmize this.
fn find_block_count_for_module_count(module_count: u16) -> Option<usize> {
    match module_count {
        1..=58 => Some(1),
        59..=121 => Some(2),
        122..=248 => Some(4),
        249..=503 => Some(8),
        504..=1014 => Some(16),
        _ => None,
    }
}

mod worldinner {
    use super::worldinnerconst::WorldInnerConst;
    use super::*;

    #[derive(Clone)]
    pub enum WorldInner {
        // One block, 64 bits, 6-bit addressing, 58 data bits
        Size1(WorldInnerConst<1>),
        // Two blocks - 128 bits, 7-bit addressing, 121 data bits
        Size2(WorldInnerConst<2>),
        // Four blocks - 256 bits, 8-bit addressing, 248 data bits
        Size4(WorldInnerConst<4>),
        // Eight blocks - 512 bits, 9-bit addressing, 503 data bits
        Size8(WorldInnerConst<8>),
        // Sixteen blocks - 1024 bits, 10-bit addressing, 1014 data bits
        Size16(WorldInnerConst<16>),
    }

    impl WorldInner {
        pub fn clone_compatible(&self, other: &Self) -> bool {
            #[allow(clippy::match_like_matches_macro)]
            match (self, other) {
                (Self::Size1(_), Self::Size1(_)) => true,
                (Self::Size2(_), Self::Size2(_)) => true,
                (Self::Size4(_), Self::Size4(_)) => true,
                (Self::Size8(_), Self::Size8(_)) => true,
                (Self::Size16(_), Self::Size16(_)) => true,
                _ => false,
            }
        }

        pub fn clone_from(&mut self, other: &Self) {
            assert!(self.clone_compatible(other));

            match (self, other) {
                (Self::Size1(self_world), Self::Size1(other_world)) => {
                    self_world.clone_from(other_world)
                }
                (Self::Size2(self_world), Self::Size2(other_world)) => {
                    self_world.clone_from(other_world)
                }
                (Self::Size4(self_world), Self::Size4(other_world)) => {
                    self_world.clone_from(other_world)
                }
                (Self::Size8(self_world), Self::Size8(other_world)) => {
                    self_world.clone_from(other_world)
                }
                (Self::Size16(self_world), Self::Size16(other_world)) => {
                    self_world.clone_from(other_world)
                }
                _ => unreachable!(),
            }
        }

        pub fn dims(&self) -> [u16; 3] {
            match self {
                Self::Size1(world) => world.dims(),
                Self::Size2(world) => world.dims(),
                Self::Size4(world) => world.dims(),
                Self::Size8(world) => world.dims(),
                Self::Size16(world) => world.dims(),
            }
        }

        pub fn module_count(&self) -> u16 {
            match self {
                Self::Size1(world) => world.module_count(),
                Self::Size2(world) => world.module_count(),
                Self::Size4(world) => world.module_count(),
                Self::Size8(world) => world.module_count(),
                Self::Size16(world) => world.module_count(),
            }
        }

        pub fn slots_modified(&self) -> bool {
            match self {
                Self::Size1(world) => world.slots_modified(),
                Self::Size2(world) => world.slots_modified(),
                Self::Size4(world) => world.slots_modified(),
                Self::Size8(world) => world.slots_modified(),
                Self::Size16(world) => world.slots_modified(),
            }
        }

        pub fn slot_count(&self) -> usize {
            match self {
                Self::Size1(world) => world.slot_count(),
                Self::Size2(world) => world.slot_count(),
                Self::Size4(world) => world.slot_count(),
                Self::Size8(world) => world.slot_count(),
                Self::Size16(world) => world.slot_count(),
            }
        }

        pub fn slot_module(&self, pos: [u16; 3], module: u16) -> bool {
            match self {
                Self::Size1(world) => world.slot_module(pos, module),
                Self::Size2(world) => world.slot_module(pos, module),
                Self::Size4(world) => world.slot_module(pos, module),
                Self::Size8(world) => world.slot_module(pos, module),
                Self::Size16(world) => world.slot_module(pos, module),
            }
        }

        pub fn slot_module_count(&self, pos: [u16; 3]) -> usize {
            match self {
                Self::Size1(world) => world.slot_module_count(pos),
                Self::Size2(world) => world.slot_module_count(pos),
                Self::Size4(world) => world.slot_module_count(pos),
                Self::Size8(world) => world.slot_module_count(pos),
                Self::Size16(world) => world.slot_module_count(pos),
            }
        }

        pub fn set_slot_module(&mut self, pos: [u16; 3], module: u16, value: bool) {
            match self {
                Self::Size1(world) => world.set_slot_module(pos, module, value),
                Self::Size2(world) => world.set_slot_module(pos, module, value),
                Self::Size4(world) => world.set_slot_module(pos, module, value),
                Self::Size8(world) => world.set_slot_module(pos, module, value),
                Self::Size16(world) => world.set_slot_module(pos, module, value),
            }
        }

        pub fn set_slot_modules(&mut self, pos: [u16; 3], value: bool) {
            match self {
                Self::Size1(world) => world.set_slot_modules(pos, value),
                Self::Size2(world) => world.set_slot_modules(pos, value),
                Self::Size4(world) => world.set_slot_modules(pos, value),
                Self::Size8(world) => world.set_slot_modules(pos, value),
                Self::Size16(world) => world.set_slot_modules(pos, value),
            }
        }

        pub fn set_slot_module_weight(&mut self, pos: [u16; 3], module: u16, weight: f32) {
            match self {
                Self::Size1(world) => world.set_slot_module_weight(pos, module, weight),
                Self::Size2(world) => world.set_slot_module_weight(pos, module, weight),
                Self::Size4(world) => world.set_slot_module_weight(pos, module, weight),
                Self::Size8(world) => world.set_slot_module_weight(pos, module, weight),
                Self::Size16(world) => world.set_slot_module_weight(pos, module, weight),
            }
        }

        pub fn set_slot_module_weights(&mut self, pos: [u16; 3], weights: &[f32]) {
            match self {
                Self::Size1(world) => world.set_slot_module_weights(pos, weights),
                Self::Size2(world) => world.set_slot_module_weights(pos, weights),
                Self::Size4(world) => world.set_slot_module_weights(pos, weights),
                Self::Size8(world) => world.set_slot_module_weights(pos, weights),
                Self::Size16(world) => world.set_slot_module_weights(pos, weights),
            }
        }

        pub fn canonicalize(&mut self) -> (bool, WorldStatus) {
            match self {
                Self::Size1(world) => world.canonicalize(),
                Self::Size2(world) => world.canonicalize(),
                Self::Size4(world) => world.canonicalize(),
                Self::Size8(world) => world.canonicalize(),
                Self::Size16(world) => world.canonicalize(),
            }
        }

        pub fn observe(&mut self, rng: &mut Rng) -> (bool, WorldStatus) {
            match self {
                Self::Size1(world) => world.observe(rng),
                Self::Size2(world) => world.observe(rng),
                Self::Size4(world) => world.observe(rng),
                Self::Size8(world) => world.observe(rng),
                Self::Size16(world) => world.observe(rng),
            }
        }

        pub fn world_status(&self) -> WorldStatus {
            match self {
                Self::Size1(world) => world.world_status(),
                Self::Size2(world) => world.world_status(),
                Self::Size4(world) => world.world_status(),
                Self::Size8(world) => world.world_status(),
                Self::Size16(world) => world.world_status(),
            }
        }
    }
}

mod worldinnerconst {
    use super::*;

    #[derive(Clone)]
    pub struct WorldInnerConst<const N: usize> {
        dims: [u16; 3],
        adjacency_rules: Vec<AdjacencyRule>,
        features: Features,

        module_count: u16,

        slots: Vec<BitVec<N>>,
        slots_modified: bool,
        slot_module_weights: Option<Vec<f32>>,

        /// Working memory for picking the nondeterministic slot with smallest
        /// entropy. Pre-allocated to maximum capacity.
        min_entropy_slots: Vec<usize>,
    }

    impl<const N: usize> WorldInnerConst<N> {
        pub fn new(
            dims: [u16; 3],
            adjacency_rules: Vec<AdjacencyRule>,
            features: Features,
        ) -> Self {
            assert!(dims[0] > 0);
            assert!(dims[1] > 0);
            assert!(dims[2] > 0);
            assert!(!adjacency_rules.is_empty());

            let mut module_count = 0;
            let mut module_max = 0;

            let mut slot = BitVec::zeros();

            for adjacency in &adjacency_rules {
                if adjacency.module_low > module_max {
                    module_max = adjacency.module_low;
                }
                if adjacency.module_high > module_max {
                    module_max = adjacency.module_high;
                }

                if slot.add(adjacency.module_low) {
                    module_count += 1;
                }
                if slot.add(adjacency.module_high) {
                    module_count += 1;
                }
            }

            assert!(module_count <= MAX_MODULE_COUNT);
            assert!(module_max < MAX_MODULE_COUNT);

            assert!(module_max + 1 == module_count);

            let slot_count = usize::from(dims[0]) * usize::from(dims[1]) * usize::from(dims[2]);
            let slots = vec![slot; slot_count];

            let slot_module_weights = if features.contains_any_weighted() {
                Some(vec![1.0; slot_count * usize::from(module_count)])
            } else {
                None
            };

            Self {
                dims,
                adjacency_rules,
                features,

                module_count,

                slots,
                slots_modified: false,
                slot_module_weights,

                min_entropy_slots: Vec::with_capacity(slot_count),
            }
        }

        pub fn clone_from(&mut self, other: &Self) {
            self.dims = other.dims;
            self.adjacency_rules.clone_from(&other.adjacency_rules);
            self.features = other.features;

            self.slots.clone_from(&other.slots);
            self.slot_module_weights
                .clone_from(&other.slot_module_weights);

            self.module_count = other.module_count;
        }

        pub fn dims(&self) -> [u16; 3] {
            self.dims
        }

        pub fn module_count(&self) -> u16 {
            self.module_count
        }

        pub fn slots_modified(&self) -> bool {
            self.slots_modified
        }

        pub fn slot_count(&self) -> usize {
            self.slots.len()
        }

        pub fn slot_module(&self, pos: [u16; 3], module: u16) -> bool {
            let index = position_to_index(self.dims, pos);
            self.slots[index].contains(module)
        }

        pub fn slot_module_count(&self, pos: [u16; 3]) -> usize {
            let index = position_to_index(self.dims, pos);
            self.slots[index].len()
        }

        pub fn set_slot_module(&mut self, pos: [u16; 3], module: u16, value: bool) {
            let index = position_to_index(self.dims, pos);
            let slot = &mut self.slots[index];

            if value {
                slot.add(module);
            } else {
                slot.remove(module);
            }

            self.slots_modified = true;
        }

        pub fn set_slot_modules(&mut self, pos: [u16; 3], value: bool) {
            let index = position_to_index(self.dims, pos);
            let slot = &mut self.slots[index];

            for module in 0..self.module_count {
                if value {
                    slot.add(module);
                } else {
                    slot.remove(module);
                }
            }

            self.slots_modified = true;
        }

        pub fn set_slot_module_weight(&mut self, pos: [u16; 3], module: u16, weight: f32) {
            assert!(module < self.module_count);
            assert!(weight.is_normal() && weight.is_sign_positive());

            if let Some(slot_module_weights) = &mut self.slot_module_weights {
                let index = usize::from(module)
                    + usize::from(self.module_count) * position_to_index(self.dims, pos);

                slot_module_weights[index] = weight;
            }
        }

        pub fn set_slot_module_weights(&mut self, pos: [u16; 3], weights: &[f32]) {
            let module_count = usize::from(self.module_count);
            assert_eq!(weights.len(), module_count);

            for weight in weights {
                assert!(weight.is_normal() && weight.is_sign_positive());
            }

            if let Some(slot_module_weights) = &mut self.slot_module_weights {
                let index_base = module_count * position_to_index(self.dims, pos);

                let module_weights =
                    &mut slot_module_weights[index_base..index_base + module_count];
                module_weights.copy_from_slice(weights);
            }
        }

        pub fn observe(&mut self, rng: &mut Rng) -> (bool, WorldStatus) {
            assert!(!self.slots_modified);

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
                        let weight_index = usize::from(self.module_count) * i + usize::from(module);
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
                    let index_base = usize::from(self.module_count) * min_entropy_slot_index;
                    let weights = &slot_module_weights
                        [index_base..index_base + usize::from(self.module_count)];

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

        pub fn canonicalize(&mut self) -> (bool, WorldStatus) {
            let mut world_changed = false;

            for i in 0..self.slots.len() {
                // Even if propagation tells us there is a contradiction, we
                // continue until we canonicalize the whole world. This is
                // because we have to set slots_modified = false.

                let (changed, _) = self.propagate_constraints(i);

                world_changed |= changed;
            }

            self.slots_modified = false;

            (world_changed, self.world_status())
        }

        fn propagate_constraints(&mut self, slot_index: usize) -> (bool, bool) {
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
                            .adjacency_rules
                            .iter()
                            .filter(|adj| adj.kind == AdjacencyRuleKind::from(s.search_direction))
                            .filter(|adj| {
                                if s.search_direction.is_positive() {
                                    slot_prev.contains(adj.module_low)
                                        && slot.contains(adj.module_high)
                                } else {
                                    slot_prev.contains(adj.module_high)
                                        && slot.contains(adj.module_low)
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

                        assert!(slot_len >= new_slot_len);
                        self.slots[s.slot_index] = new_slot;

                        if slot_len == 0 {
                            // The slot was already contradictory, stop propagating
                            // this branch. This can only happen when
                            // propagate_constraints is called from canonicalize,
                            // where it is called on every slot regardless of
                            // whether previous contradictions were found.
                            //
                            // We can therefore terminate here, because we know
                            // canonicalize will eventually call propagate
                            // constraints on every slot.
                            contradiction = true;
                            s.search_state = SearchState::Done;
                        } else if new_slot_len == 0 {
                            // We removed everything, stop the current observation iteration
                            changed = true;
                            contradiction = true;
                            s.search_state = SearchState::Done;
                        } else if slot_len == new_slot_len {
                            // We didn't remove anything, stop propagating this branch
                            s.search_state = SearchState::Done;
                        } else {
                            assert!(slot_len > new_slot_len);

                            // We removed something, propagate further
                            changed = true;
                            s.search_state = SearchState::SearchLeft;
                        }
                    }
                    SearchState::SearchLeft => {
                        let slot_index = s.slot_index;
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [prev_position_saturating(pos[0], dim_x), pos[1], pos[2]];

                        s.search_state = SearchState::SearchRight;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [next_position_saturating(pos[0], dim_x), pos[1], pos[2]];

                        s.search_state = SearchState::SearchFront;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [pos[0], prev_position_saturating(pos[1], dim_y), pos[2]];

                        s.search_state = SearchState::SearchBack;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [pos[0], next_position_saturating(pos[1], dim_y), pos[2]];

                        s.search_state = SearchState::SearchDown;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [pos[0], pos[1], prev_position_saturating(pos[2], dim_z)];

                        s.search_state = SearchState::SearchUp;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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
                        let pos = index_to_position(self.dims, slot_index);
                        let pos_next = [pos[0], pos[1], next_position_saturating(pos[2], dim_z)];

                        s.search_state = SearchState::Done;

                        if pos != pos_next {
                            let slot_index_next = position_to_index(self.dims, pos_next);
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

    fn choose_module<const N: usize>(rng: &mut Rand64, slot: &BitVec<N>) -> u16 {
        assert!(slot.len() > 0);

        let rand_num = rng.rand_u64() as usize;
        let index = rand_num % slot.len();
        slot.iter().nth(index).unwrap()
    }

    fn choose_module_weighted<const N: usize>(
        rng: &mut Rand64,
        slot: &BitVec<N>,
        weights: &[f32],
    ) -> u16 {
        assert!(slot.len() > 0);
        assert!(weights.len() <= usize::from(MAX_MODULE_COUNT));

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
        let mut cummulative_weights: ArrayVec<f32, { MAX_MODULE_COUNT as usize }> = ArrayVec::new();

        for (i, weight) in weights.iter().enumerate() {
            if slot.contains(cast_u16(i)) {
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

        cast_u16(index)
    }

    fn prev_position_saturating(pos: u16, dim: u16) -> u16 {
        assert!(dim > 0);
        pos.saturating_sub(1)
    }

    fn next_position_saturating(pos: u16, dim: u16) -> u16 {
        assert!(dim > 0);
        pos.saturating_add(1).min(dim - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_to_position_unit() {
        assert_eq!(index_to_position([1, 1, 1], 0), [0, 0, 0]);
    }

    #[test]
    fn test_position_to_index_unit() {
        assert_eq!(position_to_index([1, 1, 1], [0, 0, 0]), 0);
    }

    #[test]
    #[should_panic]
    fn test_index_to_position_unit_oob() {
        index_to_position([1, 1, 1], 1);
    }

    #[test]
    #[should_panic]
    fn test_position_to_index_unit_oob() {
        position_to_index([1, 1, 1], [0, 0, 1]);
    }

    #[test]
    fn test_find_block_count_for_module_count() {
        assert_eq!(find_block_count_for_module_count(0), None);
        assert_eq!(find_block_count_for_module_count(1), Some(1));
        assert_eq!(find_block_count_for_module_count(58), Some(1));

        assert_eq!(find_block_count_for_module_count(59), Some(2));
        assert_eq!(find_block_count_for_module_count(121), Some(2));

        assert_eq!(find_block_count_for_module_count(122), Some(4));
        assert_eq!(find_block_count_for_module_count(248), Some(4));

        assert_eq!(find_block_count_for_module_count(249), Some(8));
        assert_eq!(find_block_count_for_module_count(503), Some(8));

        assert_eq!(find_block_count_for_module_count(504), Some(16));
        assert_eq!(find_block_count_for_module_count(1014), Some(16));

        assert_eq!(find_block_count_for_module_count(1015), None);
    }
}
