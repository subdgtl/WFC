#![feature(allocator_api)]

mod bitvec;
mod convert;
mod world;

pub use crate::world::{
    index_to_position, position_to_index, AdjacencyRule, AdjacencyRuleKind, Features, Rng, World,
    WorldNewError, WorldStatus, MAX_MODULE_COUNT,
};
