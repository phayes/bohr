use std::fmt::Display;

use dimensioned::si;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    orbitals::ElectronConfiguration, periodictable::PeriodicData, periodictable::PERIODIC_TABLE,
    Block,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Atom {
    pub element: Element,

    /// Number of neutrons. May be None if number is not specified.
    pub neutrons: Option<u8>,

    /// Number of electrons.
    pub electrons: u8,
}

impl Atom {
    pub fn new(element: Element) -> Self {
        Self {
            element,
            neutrons: None,
            electrons: element.atomic_number(),
        }
    }

    #[inline(always)]
    pub fn protons(&self) -> u8 {
        self.element.atomic_number()
    }

    /// Mass number. If neutrons are unspecified, assumes the same number of neutrons as protons.
    /// TODO: Is this correct for unspecified neutrons?
    #[inline(always)]
    pub fn mass_number(&self) -> u32 {
        (self.protons() + self.neutrons.unwrap_or(self.protons())) as u32
    }

    pub fn core_electrons(&self) -> u8 {
        if self.electrons <= Element::Helium.atomic_number() {
            0
        } else if self.electrons <= Element::Neon.atomic_number() {
            Element::Helium as u8
        } else if self.electrons <= Element::Argon.atomic_number() {
            Element::Neon as u8
        } else if self.electrons <= Element::Krypton.atomic_number() {
            Element::Argon as u8
        } else if self.electrons <= Element::Xenon.atomic_number() {
            Element::Krypton as u8
        } else if self.electrons <= Element::Radon.atomic_number() {
            Element::Xenon as u8
        } else {
            Element::Radon as u8
        }
    }

    #[inline(always)]
    pub fn valence_electrons(&self) -> u8 {
        self.electrons - self.core_electrons()
    }

    #[inline(always)]
    pub fn charge_number(&self) -> i8 {
        (self.protons() as i16 - self.electrons as i16) as i8
    }

    #[inline(always)]
    pub fn is_ion(&self) -> bool {
        self.electrons != self.protons()
    }

    #[inline(always)]
    pub fn is_anion(&self) -> bool {
        self.electrons > self.protons()
    }

    #[inline(always)]
    pub fn is_cation(&self) -> bool {
        self.electrons < self.protons()
    }

    #[inline(always)]
    pub fn is_isotope(&self, other: Self) -> bool {
        self.protons() == other.protons() && self.neutrons != other.neutrons
    }

    #[inline(always)]
    pub fn is_isotone(&self, other: Self) -> bool {
        self.neutrons == other.neutrons
    }

    #[inline(always)]
    pub fn is_isobar(&self, other: Self) -> bool {
        self.mass_number() == other.mass_number()
    }

    /// Check if the two atoms are isodiaphers. If the number of neutrons is not known for either atom, returns None.
    pub fn is_isodiaphers(&self, other: Self) -> Option<bool> {
        match self.neutrons.zip(other.neutrons) {
            Some((self_neutrons, other_neutrons)) => Some(
                self_neutrons as i32 - self.protons() as i32
                    == other_neutrons as i32 - other.protons() as i32,
            ),
            None => None,
        }
    }

    /// Check if the two atoms are mirror nuclei. If the number of neutrons is not known for either atom, returns None.
    pub fn is_mirror_nuclei(&self, other: Self) -> Option<bool> {
        match self.neutrons.zip(other.neutrons) {
            Some((self_neutrons, other_neutrons)) => {
                Some(self.protons() == other_neutrons && self_neutrons == other.protons())
            }
            None => None,
        }
    }

    /// Check if the two atoms are isoelectronic (have the same electron configuration)
    pub fn is_isoelectronic(&self, other: Self) -> bool {
        if self.electrons != other.electrons {
            // If the number of electrons is different, they cannot be isoelectronic
            return false;
        } else if self.protons() < 20 && self.electrons < 20 {
            // For atoms with 20 or fewer protons and electrons, just check the number of electrons
            return self.electrons == other.electrons;
        } else if self.protons() == other.protons() && self.electrons == other.electrons {
            // If the atoms are the same (ignoring neutrons), they are isoelectronic
            return true;
        } else {
            // We actually need to check the electron configuration
            // TODO: This will currently fail for ions of over 20 protons and electrons
            return self.electron_configuration() == other.electron_configuration();
        }
    }

    pub fn electron_configuration(&self) -> ElectronConfiguration {
        ElectronConfiguration::new(self.element, self.charge_number())
    }
}

impl std::fmt::Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(neutrons) = self.neutrons {
            let mass_number = self.protons() + neutrons;
            let mass_number = format!("{}", mass_number);
            let mass_number = crate::util::superscript(mass_number);
            write!(f, "{}", mass_number)?;
        }
        write!(f, "{}", self.element.symbol())?;

        let charge_number = self.charge_number();
        if charge_number != 0 {
            let charge_number_abs = charge_number.abs();
            let mut charge_number_string = "".to_string();
            if charge_number_abs != 1 {
                charge_number_string = format!("{}", charge_number_abs);
                charge_number_string = crate::util::superscript(charge_number_string);
            }

            if charge_number < 0 {
                write!(f, "{}⁻", charge_number_string)?;
            } else {
                write!(f, "{}⁺", charge_number_string)?;
            }
        }

        Ok(())
    }
}

impl std::str::FromStr for Atom {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref ATOM_REGEX: Regex =
                Regex::new(r"^(?<isotope>[⁰¹²³⁴⁵⁶⁷⁸⁹]+)?(?<element>[A-Z][a-z]?[a-z]?)(?<charge>[⁰¹²³⁴⁵⁶⁷⁸⁹⁻⁺]+)?$").unwrap();
        }

        let captures = ATOM_REGEX.captures(s).ok_or(())?;

        let element: Element = captures.name("element").ok_or(())?.as_str().parse()?;
        let electrons = if let Some(charge) = captures.name("charge") {
            let charge = crate::util::regular_case(charge.as_str());
            let charge: i8 = match charge.as_str() {
                "-" => -1,
                "+" => 1,
                _ => {
                    if charge.ends_with("-") {
                        let abs_charge: i8 = charge[..charge.len() - 1].parse().map_err(|_| ())?;
                        -abs_charge
                    } else if charge.ends_with("+") {
                        charge[..charge.len() - 1].parse().map_err(|_| ())?
                    } else {
                        charge.parse().map_err(|_| ())?
                    }
                }
            };

            (element.atomic_number() as i16 - charge as i16) as u8
        } else {
            element.atomic_number()
        };

        let mut neutrons = None;
        if let Some(isotope) = captures.name("isotope") {
            let atomic_weight: u16 = crate::util::regular_case(isotope.as_str())
                .parse()
                .map_err(|_| ())?;
            neutrons = Some((atomic_weight - element.atomic_number() as u16) as u8);
        }

        Ok(Atom {
            element,
            neutrons,
            electrons,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Element {
    Hydrogen = 1,
    Helium,
    Lithium,
    Beryllium,
    Boron,
    Carbon,
    Nitrogen,
    Oxygen,
    Fluorine,
    Neon,
    Sodium,
    Magnesium,
    Aluminium,
    Silicon,
    Phosphorus,
    Sulfur,
    Chlorine,
    Argon,
    Potassium,
    Calcium,
    Scandium,
    Titanium,
    Vanadium,
    Chromium,
    Manganese,
    Iron,
    Cobalt,
    Nickel,
    Copper,
    Zinc,
    Gallium,
    Germanium,
    Arsenic,
    Selenium,
    Bromine,
    Krypton,
    Rubidium,
    Strontium,
    Yttrium,
    Zirconium,
    Niobium,
    Molybdenum,
    Technetium,
    Ruthenium,
    Rhodium,
    Palladium,
    Silver,
    Cadmium,
    Indium,
    Tin,
    Antimony,
    Tellurium,
    Iodine,
    Xenon,
    Cesium,
    Barium,
    Lanthanum,
    Cerium,
    Praseodymium,
    Neodymium,
    Promethium,
    Samarium,
    Europium,
    Gadolinium,
    Terbium,
    Dysprosium,
    Holmium,
    Erbium,
    Thulium,
    Ytterbium,
    Lutetium,
    Hafnium,
    Tantalum,
    Tungsten,
    Rhenium,
    Osmium,
    Iridium,
    Platinum,
    Gold,
    Mercury,
    Thallium,
    Lead,
    Bismuth,
    Polonium,
    Astatine,
    Radon,
    Francium,
    Radium,
    Actinium,
    Thorium,
    Protactinium,
    Uranium,
    Neptunium,
    Plutonium,
    Americium,
    Curium,
    Berkelium,
    Californium,
    Einsteinium,
    Fermium,
    Mendelevium,
    Nobelium,
    Lawrencium,
    Rutherfordium,
    Dubnium,
    Seaborgium,
    Bohrium,
    Hassium,
    Meitnerium,
    Darmstadtium,
    Roentgenium,
    Copernicium,
    Nihonium,
    Flerovium,
    Moscovium,
    Livermorium,
    Tennessine,
    Oganesson,
}

impl Element {
    pub fn isotope_ion(&self, mass_number: u16, charge_number: i16) -> Atom {
        let protons = self.atomic_number();
        Atom {
            element: *self,
            neutrons: Some((mass_number - protons as u16) as u8),
            electrons: (protons as i16 - charge_number) as u8,
        }
    }

    pub fn ion(&self, charge_number: i16) -> Atom {
        let protons = self.atomic_number();
        Atom {
            element: *self,
            neutrons: None,
            electrons: (protons as i16 - charge_number) as u8,
        }
    }

    pub fn isotope(&self, mass_number: u16) -> Atom {
        let protons = self.atomic_number();
        Atom {
            element: *self,
            neutrons: Some((mass_number - protons as u16) as u8),
            electrons: protons,
        }
    }

    /// Get a neutral atom for this element
    pub fn atom(&self) -> Atom {
        let protons = self.atomic_number();
        Atom {
            element: *self,
            neutrons: None,
            electrons: protons,
        }
    }

    #[inline(always)]
    pub const fn atomic_number(&self) -> u8 {
        (*self) as u8
    }

    pub fn symbol(&self) -> &str {
        match self {
            Self::Hydrogen => "H",
            Self::Helium => "He",
            Self::Lithium => "Li",
            Self::Beryllium => "Be",
            Self::Boron => "B",
            Self::Carbon => "C",
            Self::Nitrogen => "N",
            Self::Oxygen => "O",
            Self::Fluorine => "F",
            Self::Neon => "Ne",
            Self::Sodium => "Na",
            Self::Magnesium => "Mg",
            Self::Aluminium => "Al",
            Self::Silicon => "Si",
            Self::Phosphorus => "P",
            Self::Sulfur => "S",
            Self::Chlorine => "Cl",
            Self::Argon => "Ar",
            Self::Potassium => "K",
            Self::Calcium => "Ca",
            Self::Scandium => "Sc",
            Self::Titanium => "Ti",
            Self::Vanadium => "V",
            Self::Chromium => "Cr",
            Self::Manganese => "Mn",
            Self::Iron => "Fe",
            Self::Cobalt => "Co",
            Self::Nickel => "Ni",
            Self::Copper => "Cu",
            Self::Zinc => "Zn",
            Self::Gallium => "Ga",
            Self::Germanium => "Ge",
            Self::Arsenic => "As",
            Self::Selenium => "Se",
            Self::Bromine => "Br",
            Self::Krypton => "Kr",
            Self::Rubidium => "Rb",
            Self::Strontium => "Sr",
            Self::Yttrium => "Y",
            Self::Zirconium => "Zr",
            Self::Niobium => "Nb",
            Self::Molybdenum => "Mo",
            Self::Technetium => "Tc",
            Self::Ruthenium => "Ru",
            Self::Rhodium => "Rh",
            Self::Palladium => "Pd",
            Self::Silver => "Ag",
            Self::Cadmium => "Cd",
            Self::Indium => "In",
            Self::Tin => "Sn",
            Self::Antimony => "Sb",
            Self::Tellurium => "Te",
            Self::Iodine => "I",
            Self::Xenon => "Xe",
            Self::Cesium => "Cs",
            Self::Barium => "Ba",
            Self::Lanthanum => "La",
            Self::Cerium => "Ce",
            Self::Praseodymium => "Pr",
            Self::Neodymium => "Nd",
            Self::Promethium => "Pm",
            Self::Samarium => "Sm",
            Self::Europium => "Eu",
            Self::Gadolinium => "Gd",
            Self::Terbium => "Tb",
            Self::Dysprosium => "Dy",
            Self::Holmium => "Ho",
            Self::Erbium => "Er",
            Self::Thulium => "Tm",
            Self::Ytterbium => "Yb",
            Self::Lutetium => "Lu",
            Self::Hafnium => "Hf",
            Self::Tantalum => "Ta",
            Self::Tungsten => "W",
            Self::Rhenium => "Re",
            Self::Osmium => "Os",
            Self::Iridium => "Ir",
            Self::Platinum => "Pt",
            Self::Gold => "Au",
            Self::Mercury => "Hg",
            Self::Thallium => "Tl",
            Self::Lead => "Pb",
            Self::Bismuth => "Bi",
            Self::Polonium => "Po",
            Self::Astatine => "At",
            Self::Radon => "Rn",
            Self::Francium => "Fr",
            Self::Radium => "Ra",
            Self::Actinium => "Ac",
            Self::Thorium => "Th",
            Self::Protactinium => "Pa",
            Self::Uranium => "U",
            Self::Neptunium => "Np",
            Self::Plutonium => "Pu",
            Self::Americium => "Am",
            Self::Curium => "Cm",
            Self::Berkelium => "Bk",
            Self::Californium => "Cf",
            Self::Einsteinium => "Es",
            Self::Fermium => "Fm",
            Self::Mendelevium => "Md",
            Self::Nobelium => "No",
            Self::Lawrencium => "Lr",
            Self::Rutherfordium => "Rf",
            Self::Dubnium => "Db",
            Self::Seaborgium => "Sg",
            Self::Bohrium => "Bh",
            Self::Hassium => "Hs",
            Self::Meitnerium => "Mt",
            Self::Darmstadtium => "Ds",
            Self::Roentgenium => "Rg",
            Self::Copernicium => "Cn",
            Self::Nihonium => "Nh",
            Self::Flerovium => "Fl",
            Self::Moscovium => "Mc",
            Self::Livermorium => "Lv",
            Self::Tennessine => "Ts",
            Self::Oganesson => "Og",
        }
    }

    /// Get the nearest noble gas that occurs before this element.
    /// If this element is a noble gas, it will return the element directly above it in the periotic table.
    ///
    /// Useful for calculating orbitals and other things.
    pub fn nearest_noble_gas(&self) -> Option<Element> {
        if *self <= Element::Helium {
            None
        } else if *self <= Element::Neon {
            Some(Element::Helium)
        } else if *self <= Element::Argon {
            Some(Element::Neon)
        } else if *self <= Element::Krypton {
            Some(Element::Argon)
        } else if *self <= Element::Xenon {
            Some(Element::Krypton)
        } else if *self <= Element::Radon {
            Some(Element::Xenon)
        } else if *self <= Element::Oganesson {
            Some(Element::Radon)
        } else {
            panic!("Element higher than Oganesson")
        }
    }

    // TODO: Use an array instead of a vector
    pub fn all() -> Vec<Element> {
        let mut vec = Vec::with_capacity(118);
        for i in 1..=118 {
            vec.push(unsafe { std::mem::transmute(i as u8) });
        }
        vec
    }

    // Data for the element from the periodic table
    #[inline(always)]
    pub(crate) fn periodic_data(&self) -> &PeriodicData {
        PERIODIC_TABLE.get(*self)
    }

    #[inline(always)]
    pub fn name(&self) -> &str {
        self.periodic_data().name
    }

    #[inline(always)]
    pub fn period(&self) -> u8 {
        self.periodic_data().period
    }

    #[inline(always)]
    pub fn group(&self) -> u8 {
        self.periodic_data().group
    }

    #[inline(always)]
    pub fn category(&self) -> Category {
        self.periodic_data().category
    }

    #[inline(always)]
    pub fn block(&self) -> Block {
        self.periodic_data().block
    }

    #[inline(always)]
    pub fn electron_configuration(&self) -> &ElectronConfiguration {
        &self.periodic_data().electron_configuration
    }

    #[inline(always)]
    pub fn boil(&self) -> Option<si::Kelvin<f64>> {
        self.periodic_data().boil()
    }

    #[inline(always)]
    pub fn melt(&self) -> Option<si::Kelvin<f64>> {
        self.periodic_data().melt()
    }

    #[inline(always)]
    pub fn density(&self) -> Option<crate::KGPerM3<f64>> {
        self.periodic_data().density()
    }

    #[inline(always)]
    pub fn molar_heat(&self) -> Option<crate::JoulePerKelvinPerMole<f64>> {
        self.periodic_data().molar_heat()
    }

    #[inline(always)]
    pub fn electron_affinity(&self) -> Option<si::JoulePerMole<f64>> {
        self.periodic_data().electron_affinity()
    }
}

impl Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

impl TryFrom<u8> for Element {
    type Error = ();

    fn try_from(atomic_number: u8) -> Result<Self, ()> {
        if atomic_number >= 1 && atomic_number <= 118 {
            Ok(unsafe { std::mem::transmute(atomic_number) })
        } else {
            Err(())
        }
    }
}

impl std::cmp::PartialEq<u8> for Element {
    // Required method
    fn eq(&self, other: &u8) -> bool {
        self.atomic_number() == *other
    }
}

impl std::cmp::PartialOrd<u8> for Element {
    fn partial_cmp(&self, other: &u8) -> Option<std::cmp::Ordering> {
        self.atomic_number().partial_cmp(other)
    }
}

impl std::str::FromStr for Element {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "H" => Ok(Self::Hydrogen),
            "He" => Ok(Self::Helium),
            "Li" => Ok(Self::Lithium),
            "Be" => Ok(Self::Beryllium),
            "B" => Ok(Self::Boron),
            "C" => Ok(Self::Carbon),
            "N" => Ok(Self::Nitrogen),
            "O" => Ok(Self::Oxygen),
            "F" => Ok(Self::Fluorine),
            "Ne" => Ok(Self::Neon),
            "Na" => Ok(Self::Sodium),
            "Mg" => Ok(Self::Magnesium),
            "Al" => Ok(Self::Aluminium),
            "Si" => Ok(Self::Silicon),
            "P" => Ok(Self::Phosphorus),
            "S" => Ok(Self::Sulfur),
            "Cl" => Ok(Self::Chlorine),
            "Ar" => Ok(Self::Argon),
            "K" => Ok(Self::Potassium),
            "Ca" => Ok(Self::Calcium),
            "Sc" => Ok(Self::Scandium),
            "Ti" => Ok(Self::Titanium),
            "V" => Ok(Self::Vanadium),
            "Cr" => Ok(Self::Chromium),
            "Mn" => Ok(Self::Manganese),
            "Fe" => Ok(Self::Iron),
            "Co" => Ok(Self::Cobalt),
            "Ni" => Ok(Self::Nickel),
            "Cu" => Ok(Self::Copper),
            "Zn" => Ok(Self::Zinc),
            "Ga" => Ok(Self::Gallium),
            "Ge" => Ok(Self::Germanium),
            "As" => Ok(Self::Arsenic),
            "Se" => Ok(Self::Selenium),
            "Br" => Ok(Self::Bromine),
            "Kr" => Ok(Self::Krypton),
            "Rb" => Ok(Self::Rubidium),
            "Sr" => Ok(Self::Strontium),
            "Y" => Ok(Self::Yttrium),
            "Zr" => Ok(Self::Zirconium),
            "Nb" => Ok(Self::Niobium),
            "Mo" => Ok(Self::Molybdenum),
            "Tc" => Ok(Self::Technetium),
            "Ru" => Ok(Self::Ruthenium),
            "Rh" => Ok(Self::Rhodium),
            "Pd" => Ok(Self::Palladium),
            "Ag" => Ok(Self::Silver),
            "Cd" => Ok(Self::Cadmium),
            "In" => Ok(Self::Indium),
            "Sn" => Ok(Self::Tin),
            "Sb" => Ok(Self::Antimony),
            "Te" => Ok(Self::Tellurium),
            "I" => Ok(Self::Iodine),
            "Xe" => Ok(Self::Xenon),
            "Cs" => Ok(Self::Cesium),
            "Ba" => Ok(Self::Barium),
            "La" => Ok(Self::Lanthanum),
            "Ce" => Ok(Self::Cerium),
            "Pr" => Ok(Self::Praseodymium),
            "Nd" => Ok(Self::Neodymium),
            "Pm" => Ok(Self::Promethium),
            "Sm" => Ok(Self::Samarium),
            "Eu" => Ok(Self::Europium),
            "Gd" => Ok(Self::Gadolinium),
            "Tb" => Ok(Self::Terbium),
            "Dy" => Ok(Self::Dysprosium),
            "Ho" => Ok(Self::Holmium),
            "Er" => Ok(Self::Erbium),
            "Tm" => Ok(Self::Thulium),
            "Yb" => Ok(Self::Ytterbium),
            "Lu" => Ok(Self::Lutetium),
            "Hf" => Ok(Self::Hafnium),
            "Ta" => Ok(Self::Tantalum),
            "W" => Ok(Self::Tungsten),
            "Re" => Ok(Self::Rhenium),
            "Os" => Ok(Self::Osmium),
            "Ir" => Ok(Self::Iridium),
            "Pt" => Ok(Self::Platinum),
            "Au" => Ok(Self::Gold),
            "Hg" => Ok(Self::Mercury),
            "Tl" => Ok(Self::Thallium),
            "Pb" => Ok(Self::Lead),
            "Bi" => Ok(Self::Bismuth),
            "Po" => Ok(Self::Polonium),
            "At" => Ok(Self::Astatine),
            "Rn" => Ok(Self::Radon),
            "Fr" => Ok(Self::Francium),
            "Ra" => Ok(Self::Radium),
            "Ac" => Ok(Self::Actinium),
            "Th" => Ok(Self::Thorium),
            "Pa" => Ok(Self::Protactinium),
            "U" => Ok(Self::Uranium),
            "Np" => Ok(Self::Neptunium),
            "Pu" => Ok(Self::Plutonium),
            "Am" => Ok(Self::Americium),
            "Cm" => Ok(Self::Curium),
            "Bk" => Ok(Self::Berkelium),
            "Cf" => Ok(Self::Californium),
            "Es" => Ok(Self::Einsteinium),
            "Fm" => Ok(Self::Fermium),
            "Md" => Ok(Self::Mendelevium),
            "No" => Ok(Self::Nobelium),
            "Lr" => Ok(Self::Lawrencium),
            "Rf" => Ok(Self::Rutherfordium),
            "Db" => Ok(Self::Dubnium),
            "Sg" => Ok(Self::Seaborgium),
            "Bh" => Ok(Self::Bohrium),
            "Hs" => Ok(Self::Hassium),
            "Mt" => Ok(Self::Meitnerium),
            "Ds" => Ok(Self::Darmstadtium),
            "Rg" => Ok(Self::Roentgenium),
            "Cn" => Ok(Self::Copernicium),
            "Nh" => Ok(Self::Nihonium),
            "Fl" => Ok(Self::Flerovium),
            "Mc" => Ok(Self::Moscovium),
            "Lv" => Ok(Self::Livermorium),
            "Ts" => Ok(Self::Tennessine),
            "Og" => Ok(Self::Oganesson),
            "Uuo" => Ok(Self::Oganesson),
            "Hydrogen" => Ok(Self::Hydrogen),
            "Helium" => Ok(Self::Helium),
            "Lithium" => Ok(Self::Lithium),
            "Beryllium" => Ok(Self::Beryllium),
            "Boron" => Ok(Self::Boron),
            "Carbon" => Ok(Self::Carbon),
            "Nitrogen" => Ok(Self::Nitrogen),
            "Oxygen" => Ok(Self::Oxygen),
            "Fluorine" => Ok(Self::Fluorine),
            "Neon" => Ok(Self::Neon),
            "Sodium" => Ok(Self::Sodium),
            "Magnesium" => Ok(Self::Magnesium),
            "Aluminium" => Ok(Self::Aluminium),
            "Silicon" => Ok(Self::Silicon),
            "Phosphorus" => Ok(Self::Phosphorus),
            "Sulfur" => Ok(Self::Sulfur),
            "Chlorine" => Ok(Self::Chlorine),
            "Argon" => Ok(Self::Argon),
            "Potassium" => Ok(Self::Potassium),
            "Calcium" => Ok(Self::Calcium),
            "Scandium" => Ok(Self::Scandium),
            "Titanium" => Ok(Self::Titanium),
            "Vanadium" => Ok(Self::Vanadium),
            "Chromium" => Ok(Self::Chromium),
            "Manganese" => Ok(Self::Manganese),
            "Iron" => Ok(Self::Iron),
            "Cobalt" => Ok(Self::Cobalt),
            "Nickel" => Ok(Self::Nickel),
            "Copper" => Ok(Self::Copper),
            "Zinc" => Ok(Self::Zinc),
            "Gallium" => Ok(Self::Gallium),
            "Germanium" => Ok(Self::Germanium),
            "Arsenic" => Ok(Self::Arsenic),
            "Selenium" => Ok(Self::Selenium),
            "Bromine" => Ok(Self::Bromine),
            "Krypton" => Ok(Self::Krypton),
            "Rubidium" => Ok(Self::Rubidium),
            "Strontium" => Ok(Self::Strontium),
            "Yttrium" => Ok(Self::Yttrium),
            "Zirconium" => Ok(Self::Zirconium),
            "Niobium" => Ok(Self::Niobium),
            "Molybdenum" => Ok(Self::Molybdenum),
            "Technetium" => Ok(Self::Technetium),
            "Ruthenium" => Ok(Self::Ruthenium),
            "Rhodium" => Ok(Self::Rhodium),
            "Palladium" => Ok(Self::Palladium),
            "Silver" => Ok(Self::Silver),
            "Cadmium" => Ok(Self::Cadmium),
            "Indium" => Ok(Self::Indium),
            "Tin" => Ok(Self::Tin),
            "Antimony" => Ok(Self::Antimony),
            "Tellurium" => Ok(Self::Tellurium),
            "Iodine" => Ok(Self::Iodine),
            "Xenon" => Ok(Self::Xenon),
            "Cesium" => Ok(Self::Cesium),
            "Barium" => Ok(Self::Barium),
            "Lanthanum" => Ok(Self::Lanthanum),
            "Cerium" => Ok(Self::Cerium),
            "Praseodymium" => Ok(Self::Praseodymium),
            "Neodymium" => Ok(Self::Neodymium),
            "Promethium" => Ok(Self::Promethium),
            "Samarium" => Ok(Self::Samarium),
            "Europium" => Ok(Self::Europium),
            "Gadolinium" => Ok(Self::Gadolinium),
            "Terbium" => Ok(Self::Terbium),
            "Dysprosium" => Ok(Self::Dysprosium),
            "Holmium" => Ok(Self::Holmium),
            "Erbium" => Ok(Self::Erbium),
            "Thulium" => Ok(Self::Thulium),
            "Ytterbium" => Ok(Self::Ytterbium),
            "Lutetium" => Ok(Self::Lutetium),
            "Hafnium" => Ok(Self::Hafnium),
            "Tantalum" => Ok(Self::Tantalum),
            "Tungsten" => Ok(Self::Tungsten),
            "Rhenium" => Ok(Self::Rhenium),
            "Osmium" => Ok(Self::Osmium),
            "Iridium" => Ok(Self::Iridium),
            "Platinum" => Ok(Self::Platinum),
            "Gold" => Ok(Self::Gold),
            "Mercury" => Ok(Self::Mercury),
            "Thallium" => Ok(Self::Thallium),
            "Lead" => Ok(Self::Lead),
            "Bismuth" => Ok(Self::Bismuth),
            "Polonium" => Ok(Self::Polonium),
            "Astatine" => Ok(Self::Astatine),
            "Radon" => Ok(Self::Radon),
            "Francium" => Ok(Self::Francium),
            "Radium" => Ok(Self::Radium),
            "Actinium" => Ok(Self::Actinium),
            "Thorium" => Ok(Self::Thorium),
            "Protactinium" => Ok(Self::Protactinium),
            "Uranium" => Ok(Self::Uranium),
            "Neptunium" => Ok(Self::Neptunium),
            "Plutonium" => Ok(Self::Plutonium),
            "Americium" => Ok(Self::Americium),
            "Curium" => Ok(Self::Curium),
            "Berkelium" => Ok(Self::Berkelium),
            "Californium" => Ok(Self::Californium),
            "Einsteinium" => Ok(Self::Einsteinium),
            "Fermium" => Ok(Self::Fermium),
            "Mendelevium" => Ok(Self::Mendelevium),
            "Nobelium" => Ok(Self::Nobelium),
            "Lawrencium" => Ok(Self::Lawrencium),
            "Rutherfordium" => Ok(Self::Rutherfordium),
            "Dubnium" => Ok(Self::Dubnium),
            "Seaborgium" => Ok(Self::Seaborgium),
            "Bohrium" => Ok(Self::Bohrium),
            "Hassium" => Ok(Self::Hassium),
            "Meitnerium" => Ok(Self::Meitnerium),
            "Darmstadtium" => Ok(Self::Darmstadtium),
            "Roentgenium" => Ok(Self::Roentgenium),
            "Copernicium" => Ok(Self::Copernicium),
            "Nihonium" => Ok(Self::Nihonium),
            "Flerovium" => Ok(Self::Flerovium),
            "Moscovium" => Ok(Self::Moscovium),
            "Livermorium" => Ok(Self::Livermorium),
            "Tennessine" => Ok(Self::Tennessine),
            "Oganesson" => Ok(Self::Oganesson),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Category {
    #[serde(rename = "actinide")]
    Actinide,

    #[serde(rename = "alkali metal")]
    AlkaliMetal,

    #[serde(rename = "alkaline earth metal")]
    AlkalineEarthMetal,

    #[serde(rename = "aiatomic nonmetal")]
    AiatomicNonmetal,

    #[serde(rename = "diatomic nonmetal")]
    DiatomicNonmetal,

    #[serde(rename = "noble gas")]
    NobleGas,

    #[serde(rename = "metalloid")]
    Metalloid,

    #[serde(rename = "polyatomic nonmetal")]
    PolyatomicNonmetal,

    #[serde(rename = "post-transition metal")]
    PostTransitionMetal,

    #[serde(rename = "lanthanide")]
    Lanthanide,

    #[serde(rename = " transition metal")]
    TransitionMetal,
}

impl std::str::FromStr for Category {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "actinide" => Ok(Self::Actinide),
            "alkali metal" => Ok(Self::AlkaliMetal),
            "alkaline earth metal" => Ok(Self::AlkalineEarthMetal),
            "aiatomic nonmetal" => Ok(Self::AiatomicNonmetal),
            "noble gas" => Ok(Self::NobleGas),
            "metalloid" => Ok(Self::Metalloid),
            "polyatomic nonmetal" => Ok(Self::PolyatomicNonmetal),
            "post-transition metal" => Ok(Self::PostTransitionMetal),
            "lanthanide" => Ok(Self::Lanthanide),
            "transition metal" => Ok(Self::TransitionMetal),
            "diatomic nonmetal" => Ok(Self::DiatomicNonmetal),
            _ => Err(()),
        }
    }
}

impl From<&str> for Category {
    fn from(s: &str) -> Self {
        s.parse().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atom_display() {
        let hydrogen = Element::Hydrogen;

        let hydrogen_atom = hydrogen.atom();
        assert_eq!(hydrogen_atom.is_ion(), false);
        assert_eq!(format!("{}", hydrogen_atom), "H");
        assert_eq!("H".parse::<Atom>().unwrap(), hydrogen_atom);

        let hydrogen_isotope = hydrogen.isotope(1);
        assert_eq!(hydrogen_isotope.is_ion(), false);
        assert_eq!(format!("{}", hydrogen_isotope), "¹H");
        assert_eq!("¹H".parse::<Atom>().unwrap(), hydrogen_isotope);

        let deuterium = Element::Hydrogen.isotope(2);
        assert_eq!(deuterium.is_ion(), false);
        assert_eq!(format!("{}", deuterium), "²H");
        assert_eq!("²H".parse::<Atom>().unwrap(), deuterium);

        let tritium = Element::Hydrogen.isotope(3);
        assert_eq!(tritium.is_ion(), false);
        assert_eq!(format!("{}", tritium), "³H");
        assert_eq!("³H".parse::<Atom>().unwrap(), tritium);

        let hydrogen_anion = hydrogen.ion(-1);
        assert_eq!(hydrogen_anion.is_ion(), true);
        assert_eq!(hydrogen_anion.is_anion(), true);
        assert_eq!(hydrogen_anion.is_cation(), false);
        assert_eq!(format!("{}", hydrogen_anion), "H⁻");
        assert_eq!("H⁻".parse::<Atom>().unwrap(), hydrogen_anion);
        assert_eq!("H¹⁻".parse::<Atom>().unwrap(), hydrogen_anion);
        assert_eq!("H⁻¹".parse::<Atom>().unwrap(), hydrogen_anion);

        let hydrogen_cation = hydrogen.ion(1);
        assert_eq!(hydrogen_cation.is_ion(), true);
        assert_eq!(hydrogen_cation.is_anion(), false);
        assert_eq!(hydrogen_cation.is_cation(), true);
        assert_eq!(format!("{}", hydrogen_cation), "H⁺");
        assert_eq!("H⁺".parse::<Atom>().unwrap(), hydrogen_cation);
        assert_eq!("H¹⁺".parse::<Atom>().unwrap(), hydrogen_cation);
        assert_eq!("H⁺¹".parse::<Atom>().unwrap(), hydrogen_cation);

        let hydrogen_isotope_ion = hydrogen.isotope_ion(2, -1);
        assert_eq!(hydrogen_isotope_ion.is_ion(), true);
        assert_eq!(hydrogen_isotope_ion.is_anion(), true);
        assert_eq!(hydrogen_isotope_ion.is_cation(), false);
        assert_eq!(format!("{}", hydrogen_isotope_ion), "²H⁻");
        assert_eq!("²H⁻".parse::<Atom>().unwrap(), hydrogen_isotope_ion);
        assert_eq!("²H¹⁻".parse::<Atom>().unwrap(), hydrogen_isotope_ion);
        assert_eq!("²H⁻¹".parse::<Atom>().unwrap(), hydrogen_isotope_ion);

        let magnesium = Element::Magnesium;
        let magnesium_atom = magnesium.atom();
        assert_eq!(format!("{}", magnesium_atom), "Mg");
        assert_eq!("Mg".parse::<Atom>().unwrap(), magnesium_atom);

        let magnesium_isotope = magnesium.isotope(24);
        assert_eq!(format!("{}", magnesium_isotope), "²⁴Mg");
        assert_eq!("²⁴Mg".parse::<Atom>().unwrap(), magnesium_isotope);

        let magnesium_anion = magnesium.ion(-2);
        assert_eq!(magnesium_anion.is_ion(), true);
        assert_eq!(magnesium_anion.is_anion(), true);
        assert_eq!(magnesium_anion.is_cation(), false);
        assert_eq!(format!("{}", magnesium_anion), "Mg²⁻");
        assert_eq!("Mg²⁻".parse::<Atom>().unwrap(), magnesium_anion);
        assert_eq!("Mg⁻²".parse::<Atom>().unwrap(), magnesium_anion);

        let magnesium_cation = magnesium.ion(2);
        assert_eq!(magnesium_cation.is_ion(), true);
        assert_eq!(magnesium_cation.is_anion(), false);
        assert_eq!(magnesium_cation.is_cation(), true);
        assert_eq!(format!("{}", magnesium_cation), "Mg²⁺");
        assert_eq!("Mg²⁺".parse::<Atom>().unwrap(), magnesium_cation);
        assert_eq!("Mg⁺²".parse::<Atom>().unwrap(), magnesium_cation);
    }

    #[test]
    pub fn test_valence_electron() {
        for element in Element::all() {
            let atom = element.atom();
            let electron_configuration = element.electron_configuration();
            assert_eq!(
                electron_configuration.valence_electrons(),
                atom.valence_electrons(),
                "{} mismatch in number of valence electrons",
                element
            );
        }
    }
}
