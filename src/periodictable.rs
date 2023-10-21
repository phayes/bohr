use serde::{Deserialize, Serialize};
use serde_big_array::BigArray;
use lazy_static::lazy_static;
extern crate dimensioned as dim;
use dim::si;
use dim::typenum::Quot;
use dimensioned::f64prefixes::KILO;

type JoulePerKelvinPerMole = Quot<si::JoulePerKelvin<f64>, si::Mole<f64>>;
type KGPerM3 = Quot<si::Kilogram<f64>, si::Meter3<f64>>;
pub use dimensioned::unit_systems::si::f64consts::EV;

use crate::Element;
use crate::orbitals::ElectronConfiguration;

#[derive(Serialize, Deserialize, Clone)]
pub struct PeriodicData<'a> {
    pub name: &'a str,
    pub appearance: Option<&'a str>,
    pub atomic_mass: f64,
    boil: Option<f64>,
    pub category: crate::Category,
    density: Option<f64>,
    melt: Option<f64>,
    molar_heat: Option<f64>,
    pub number: u8,
    pub period: u8,
    pub group: u8,
    pub phase: &'a str,
    pub source: &'a str,
    pub bohr_model_image: Option<&'a str>,
    pub bohr_model_3d: Option<&'a str>,
    pub spectral_img: Option<&'a str>,
    pub symbol: &'a str,
    pub xpos: u8,
    pub ypos: u8,
    pub wxpos: u8,
    pub wypos: u8,
    pub shells: Vec<u32>,
    pub electron_configuration: ElectronConfiguration,
    pub electron_affinity: Option<f64>,
    pub electronegativity_pauling: Option<f64>,
    pub ionization_energies: Vec<f64>,
    pub cpk_hex: Option<&'a str>,
    pub image: PeriodicImage<'a>,
    pub block: crate::Block,
}

impl<'a> PeriodicData<'a> {
    // Boiling point in Kelvin
    pub fn boil(&self) -> Option<si::Kelvin<f64>> {
        match self.boil {
            Some(boil) => Some(boil * si::K),
            None => None,
        }
    }

    // Melting point in Kelvin
    pub fn melt(&self) -> Option<si::Kelvin<f64>> {
        match self.melt {
            Some(melt) => Some(melt * si::K),
            None => None,
        }
    }

    // Density (at STP) in kg/m^3 (equivilent to g/L)
    pub fn density(&self) -> Option<KGPerM3> {
        match self.density {
            Some(elem_density) => Some(elem_density * si::KG / si::M3),
            None => None,
        }
    }

    pub fn molar_heat(&self) -> Option<JoulePerKelvinPerMole> {
        match self.molar_heat {
            Some(molar_heat) => Some(molar_heat * si::J / si::K / si::MOL),
            None => None,
        }
    }

    pub fn electron_affinity(&self) -> Option<si::JoulePerMole<f64>> {
        match self.electron_affinity {
            Some(electron_affinity) => Some(electron_affinity * KILO * si::J / si::MOL),
            None => None,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
pub struct PeriodicImage<'a> {
    pub title: &'a str,
    pub url: &'a str,
    pub attribution: &'a str,
}

#[derive(Serialize, Deserialize)]
pub struct PeriodicTable<'a> {
    #[serde(with = "BigArray")]
    #[serde(borrow = "'a")]
    elements: [PeriodicData<'a>; 119],
}

impl PeriodicTable<'_> {
    #[inline(always)]
    pub fn get(&self, element: Element) -> &PeriodicData {
        unsafe { self.elements.get_unchecked(element.atomic_number() as usize - 1) }
    }
}

lazy_static! {
    pub static ref PERIODIC_TABLE: PeriodicTable<'static> = {
        let table: PeriodicTable = include!(concat!(env!("OUT_DIR"), "/periodic_table_data.rs"));
        table
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use approx::assert_abs_diff_eq;

    #[test]
    fn hydrogen_props() {
        let hydrogen = PERIODIC_TABLE.get(Element::Hydrogen);

        // Density
        assert_eq!(hydrogen.density(), Some(0.08988 * si::KG / si::M3));

        // Boiling point
        assert_eq!(hydrogen.boil(), Some(20.271 * si::K));

        // Melting point
        assert_eq!(hydrogen.melt(), Some(13.99 * si::K));

        // Molar heat
        assert_eq!(hydrogen.molar_heat(), Some(28.836 * si::J / si::K / si::MOL));

        // Electron affinity
        assert_eq!(hydrogen.electron_affinity(), Some(72.769 * KILO * si::J / si::MOL));
        assert_eq!(hydrogen.electron_affinity(), Some(72769.0 * si::J / si::MOL));

        let electron_volt: si::JoulePerMole<f64> = 96.486 * KILO * si::J / si::MOL;
        assert_abs_diff_eq!(hydrogen.electron_affinity().unwrap().value_unsafe, (0.754195 * electron_volt).value_unsafe, epsilon = 1.0);
    }
}