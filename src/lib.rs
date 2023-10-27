//! Bohr is a general purpose chemistry library for Rust.
//!
//! It is currently a personal vehicle for myself to learn undergraduate-level chemistry. 
//! It is not currently reocommended for use in production. 
mod element;
mod periodictable;
pub mod orbitals;
pub mod simple_molecule;

use dimensioned::{si, typenum::Quot};
pub use element::*;
pub(crate) mod util;
// pub mod reactant;

pub use element::Category;

pub type JoulePerKelvinPerMole<T> = Quot<si::JoulePerKelvin<T>, si::Mole<T>>;
pub type KGPerM3<T> = Quot<si::Kilogram<T>, si::Meter3<T>>;

/// Common enums
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
#[repr(u8)]
pub enum Block {
    /// l = 0, Sharp
    S = 0,
    /// l = 1, Principal
    P,
    /// l = 2, Diffuse
    D,
    /// l = 3, Fundamental
    F,
    /// l = 4, Not used for neutral elements, but useful for highly negative ions
    G,
    /// l = 5, Not used for neutral elements, but useful for highly negative ions
    H,
}

impl std::str::FromStr for Block {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "s" => Ok(Block::S),
            "p" => Ok(Block::P),
            "d" => Ok(Block::D),
            "f" => Ok(Block::F),
            "g" => Ok(Block::G),
            "h" => Ok(Block::H),
            _ => Err(()),
        }
    }
}

impl From<&str> for Block {
    fn from(s: &str) -> Self {
        s.parse().unwrap()
    }
}
