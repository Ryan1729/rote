/// The purpose of this crate is to make switching hash algorithms across the whole 
/// app easy.
pub use rustc_hash::{
    FxHasher as Hasher, 
    FxHashMap as Map, 
    FxHashSet as Set
};
