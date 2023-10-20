use crate::Element;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::num::NonZeroU8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Subshell {
    /// Quantum number
    pub n: NonZeroU8,

    ///  Orbital Angular Momentum Quantum Number (aka Block)
    pub l: u8,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Orbital {
    /// Quantum number
    pub n: NonZeroU8,

    ///  Orbital Angular Momentum Quantum Number (aka Block)
    pub l: u8,

    /// Magnetic Quantum Number
    pub m: i8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FilledSubshell {
    /// The orbital set the electrons are in
    pub subshell: Subshell,

    /// The number of electrons in the orbital set, from zero to subshell.electron_capacity()
    pub num_electrons: u8,
}

impl FilledSubshell {
    pub fn is_full(&self) -> bool {
        self.num_electrons == self.subshell.electron_capacity()
    }

    pub fn is_empty(&self) -> bool {
        self.num_electrons == 0
    }

    /// Calculate number of full orbitals using Hund's rule
    pub fn num_full_orbitals(&self) -> u8 {
        let num_orbitals = self.subshell.num_orbitals();
        if self.num_electrons < num_orbitals {
            return 0;
        }

        // Everything is at least half-full, how many electrons left to fill up?
        let remaining_electrons = self.num_electrons - self.subshell.num_orbitals();
        remaining_electrons
    }

    /// Calculate number of empty orbitals using Hund's rule
    pub fn num_empty_orbitals(&self) -> u8 {
        let num_orbitals = self.subshell.num_orbitals();
        if self.num_electrons > num_orbitals {
            return 0;
        }
        num_orbitals - self.num_electrons
    }

    /// Calculate the number of unpaired electrons inhabiting half-filled orbitals using Hund's rule
    pub fn unpaired_electrons(&self) -> u8 {
        if self.num_electrons < self.subshell.num_orbitals() {
            return self.num_electrons;
        }
        // Everything is at least half-full, how many electrons left to fill up?
        let remaining_electrons = self.num_electrons - self.subshell.num_orbitals();
        self.subshell.num_orbitals() - remaining_electrons
    }
}

// FilledSubshell are ordered first in terms of their quantum number, then in terms of subshell (l), then in terms of num electrons
impl std::cmp::PartialOrd for FilledSubshell {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.subshell.n == other.subshell.n {
            if self.subshell.l == other.subshell.l {
                self.num_electrons.partial_cmp(&other.num_electrons)
            } else {
                self.subshell.l.partial_cmp(&other.subshell.l)
            }
        } else {
            self.subshell.n.partial_cmp(&other.subshell.n)
        }
    }
}

impl std::cmp::Ord for FilledSubshell {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::fmt::Display for FilledSubshell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.subshell,
            crate::util::superscript_number(self.num_electrons)
        )
    }
}

impl std::str::FromStr for FilledSubshell {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref FILLED_SUBSHELL_REGEX: Regex =
                Regex::new(r"^(?<quantum_number>[1-9])(?<block>[spdfgh])(?<electrons>[0-9⁰¹²³⁴⁵⁶⁷⁸⁹][0-9⁰¹²³⁴⁵⁶⁷⁸⁹]?)$").unwrap();
        }

        let caps = FILLED_SUBSHELL_REGEX.captures(s).ok_or(())?;
        let quantum_number: u8 = caps["quantum_number"].parse().map_err(|_| ())?;

        let block: u8 = match &caps["block"] {
            "s" => 0,
            "p" => 1,
            "d" => 2,
            "f" => 3,
            "g" => 4,
            "h" => 5,
            _ => panic!("Invalid orbital letter"),
        };
        // TODO: Do a better job of this, actually transform superscript chars to regular and parse
        let electrons: u8 = match &caps["electrons"] {
            "⁰" => 0,
            "¹" => 1,
            "²" => 2,
            "³" => 3,
            "⁴" => 4,
            "⁵" => 5,
            "⁶" => 6,
            "⁷" => 7,
            "⁸" => 8,
            "⁹" => 9,
            "¹⁰" => 10,
            "¹¹" => 11,
            "¹²" => 12,
            "¹³" => 13,
            "¹⁴" => 14,
            "¹⁵" => 15,
            "¹⁶" => 16,
            "0" => 0,
            "1" => 1,
            "2" => 2,
            "3" => 3,
            "4" => 4,
            "5" => 5,
            "6" => 6,
            "7" => 7,
            "8" => 8,
            "9" => 9,
            "10" => 10,
            "11" => 11,
            "12" => 12,
            "13" => 13,
            "14" => 14,
            "15" => 15,
            "16" => 16,
            _ => panic!("Invalid electron count"),
        };

        Ok(Self {
            subshell: Subshell::new(quantum_number, block),
            num_electrons: electrons,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
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

impl Subshell {
    pub const fn new(n: u8, l: u8) -> Self {
        if n == 0 {
            panic!("n cannot be zero");
        }

        Self {
            n: unsafe { NonZeroU8::new_unchecked(n) },
            l,
        }
    }

    pub fn quantum_number(&self) -> u8 {
        self.n.get()
    }

    pub fn shell_index(&self) -> usize {
        (self.n.get() - 1) as usize
    }

    pub fn block(&self) -> Block {
        match self.l {
            0 => Block::S,
            1 => Block::P,
            2 => Block::D,
            3 => Block::F,
            4 => Block::G,
            5 => Block::H,
            _ => panic!("Invalid l value"),
        }
    }

    pub fn num_orbitals(&self) -> u8 {
        match self.l {
            0 => 1,
            1 => 3,
            2 => 5,
            3 => 7,
            4 => 9,
            5 => 11,
            _ => panic!("Invalid l value"),
        }
    }

    pub fn electron_capacity(&self) -> u8 {
        self.num_orbitals() * 2
    }

    pub fn orbitals(&self) -> Vec<Orbital> {
        let mut orbitals = Vec::with_capacity(self.num_orbitals() as usize);
        let l = self.l as i8;
        for m in -l..=l {
            orbitals.push(Orbital {
                n: self.n,
                l: self.l,
                m,
            });
        }
        orbitals
    }
}

impl std::fmt::Display for Subshell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.n,
            match self.l {
                0 => "s",
                1 => "p",
                2 => "d",
                3 => "f",
                4 => "g",
                5 => "h",
                _ => panic!("Invalid l value"),
            }
        )
    }
}

impl std::str::FromStr for Subshell {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let n = chars.next().unwrap().to_digit(10).unwrap() as u8;
        let l: u8 = match chars.next().unwrap() {
            's' => 0,
            'p' => 1,
            'd' => 2,
            'f' => 3,
            'g' => 4,
            'h' => 5,
            _ => panic!("Invalid orbital letter"),
        };
        Ok(Subshell::new(n, l))
    }
}

impl Orbital {
    pub fn new(n: u8, l: u8, m: i8) -> Self {
        Self {
            n: NonZeroU8::new(n).unwrap(),
            l,
            m,
        }
    }
}

/// All subshells in aufbau fill order.
/// 1s, 2s, 2p, 3s, 3p, 4s, 3d, 4p, 5s, 4d, 5p, 6s, 4f, 5d, 6p, 7s, 5f, 6d, 7p
///
/// See https://edu.rsc.org/feature/the-trouble-with-the-aufbau-principle/2000133.article for places where aufbau fails.
const AUFBAU_SUBSHELLS: [Subshell; 19] = [
    Subshell::new(1, 0), // 1s
    Subshell::new(2, 0), // 2s
    Subshell::new(2, 1), // 2p
    Subshell::new(3, 0), // 3s
    Subshell::new(3, 1), // 3p
    Subshell::new(4, 0), // 4s
    Subshell::new(3, 2), // 3d
    Subshell::new(4, 1), // 4p
    Subshell::new(5, 0), // 5s
    Subshell::new(4, 2), // 4d
    Subshell::new(5, 1), // 5p
    Subshell::new(6, 0), // 6s
    Subshell::new(4, 3), // 4f
    Subshell::new(5, 2), // 5d
    Subshell::new(6, 1), // 6p
    Subshell::new(7, 0), // 7s
    Subshell::new(5, 3), // 5f
    Subshell::new(6, 2), // 6d
    Subshell::new(7, 1), // 7p
];

lazy_static! {
    static ref ANOMALOUS_NEUTRAL_ELECTRON_CONFIGURATIONS: HashMap<Element, ElectronConfiguration> = {
        let mut map = HashMap::with_capacity(20);

        map.insert(
            Element::Chromium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d⁵ 4s¹".parse().unwrap(),
        );
        map.insert(
            Element::Copper,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s¹".parse().unwrap(),
        );
        map.insert(
            Element::Niobium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d⁴ 5s¹".parse().unwrap(),
        );
        map.insert(
            Element::Molybdenum,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d5 5s1".parse().unwrap(),
        );
        map.insert(
            Element::Ruthenium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d7 5s1".parse().unwrap(),
        );
        map.insert(
            Element::Rhodium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d8 5s1".parse().unwrap(),
        );
        map.insert(
            Element::Palladium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d10".parse().unwrap(),
        );
        map.insert(
            Element::Silver,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d10 5s1".parse().unwrap(),
        );
        map.insert(
            Element::Lanthanum,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 5d1 6s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Cerium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f1 5d1 6s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Gadolinium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f7 5d1 6s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Platinum,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d9 6s1"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Gold,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s1"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Actinium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 6d1 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Thorium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 6d2 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Protactinium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 5f2 6d1 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Uranium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 5f3 6d1 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Neptunium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 5f4 6d1 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Curium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 4f14 5d10 6s2 6p6 5f7 6d1 7s2"
                .parse()
                .unwrap(),
        );
        map.insert(
            Element::Lawrencium,
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 4f¹⁴ 5s² 5p⁶ 5d¹⁰ 5f¹⁴ 6s² 6p⁶ 7s² 7p¹"
                .parse()
                .unwrap(),
        );

        map
    };
    static ref NOBLE_GAS_ELECTRON_CONFIGURATIONS: Vec<(Element, ElectronConfiguration)> = {
        let mut nobles = Vec::with_capacity(7);

        nobles.push((
            Element::Oganesson,
            ElectronConfiguration::aufbau(Element::Oganesson.atomic_number()),
        ));
        nobles.push((
            Element::Radon,
            ElectronConfiguration::aufbau(Element::Radon.atomic_number()),
        ));
        nobles.push((
            Element::Xenon,
            ElectronConfiguration::aufbau(Element::Xenon.atomic_number()),
        ));
        nobles.push((
            Element::Krypton,
            ElectronConfiguration::aufbau(Element::Krypton.atomic_number()),
        ));
        nobles.push((
            Element::Argon,
            ElectronConfiguration::aufbau(Element::Argon.atomic_number()),
        ));
        nobles.push((
            Element::Neon,
            ElectronConfiguration::aufbau(Element::Neon.atomic_number()),
        ));
        nobles.push((
            Element::Helium,
            ElectronConfiguration::aufbau(Element::Helium.atomic_number()),
        ));

        nobles
    };
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ElectronConfiguration {
    pub subshells: Vec<FilledSubshell>,
}

impl ElectronConfiguration {
    pub fn new(element: Element, electrons: u8) -> Self {
        // If it's a neutral element, use aufbau with known exceptions
        if electrons == element.atomic_number() {
            if electrons < Element::Scandium.atomic_number() {
                Self::aufbau(electrons)
            } else {
                if let Some(electron_config) =
                    ANOMALOUS_NEUTRAL_ELECTRON_CONFIGURATIONS.get(&element)
                {
                    electron_config.clone()
                } else {
                    Self::aufbau(electrons)
                }
            }
        }
        // If it's an ion, we can use aufbau for lower elements, otherwise it's still a TODO
        else {
            if element < Element::Scandium && electrons < Element::Scandium.atomic_number() {
                Self::aufbau(electrons)
            } else {
                todo!("Handle ionic electron configurations")
            }
        }
    }

    /// Construct an electron configuration from a number of electrons using aufbau theory.
    ///
    /// Aufbau has many shortcomings and may not be suitable for all elements. See https://edu.rsc.org/feature/the-trouble-with-the-aufbau-principle/2000133.article .
    pub fn aufbau(num_electrons: u8) -> Self {
        let mut filled_subshells = Vec::new();
        let mut remaining_electrons = num_electrons;
        for subshell in AUFBAU_SUBSHELLS {
            let electron_capacity = subshell.electron_capacity();
            if remaining_electrons >= electron_capacity {
                filled_subshells.push(FilledSubshell {
                    subshell,
                    num_electrons: electron_capacity,
                });
                remaining_electrons -= electron_capacity;
            } else if remaining_electrons > 0 {
                filled_subshells.push(FilledSubshell {
                    subshell,
                    num_electrons: remaining_electrons,
                });
                remaining_electrons = 0;
            } else {
                break;
            }
        }

        if filled_subshells.is_empty() {
            // Place a single empty 1s subshell
            filled_subshells.push(FilledSubshell {
                subshell: Subshell::new(1, 0),
                num_electrons: 0,
            });
        }

        // Sort in shell order, even if they are not filled in that order
        filled_subshells.sort();

        Self {
            subshells: filled_subshells,
        }
    }

    pub fn last_subshell(&self) -> FilledSubshell {
        *self.subshells.last().unwrap()
    }

    pub fn num_electrons(&self) -> u8 {
        self.subshells
            .iter()
            .map(|filled_subshell| filled_subshell.num_electrons)
            .sum()
    }

    pub fn valence_electrons(&self) -> u8 {
        if self.num_electrons() >= 21 {
            todo!("Handle valence for configurations with more than 20 electrons")
        }

        // Find the quantum number of the last subshell, and add up all electrons in all subshells that share that quantum number
        let last_n = self.last_subshell().subshell.n;
        self.subshells
            .iter()
            .filter(|filled_subshell| filled_subshell.subshell.n == last_n)
            .map(|filled_subshell| filled_subshell.num_electrons)
            .sum()
    }

    pub fn into_subshells(self) -> Vec<FilledSubshell> {
        self.subshells
    }

    pub fn full_subshells(&self) -> Vec<&FilledSubshell> {
        self.subshells
            .iter()
            .filter(|filled_subshell| filled_subshell.is_full())
            .collect()
    }

    pub fn len(&self) -> usize {
        self.subshells.len()
    }

    /// Check if self contains the other electron configuration
    pub fn contains(&self, other: &Self) -> bool {
        if other.len() > self.len() {
            return false;
        }

        let mut contains = true;
        for shell in &other.subshells {
            if self.subshells.contains(shell) {
                continue;
            } else {
                contains = false;
                break;
            }
        }

        contains
    }

    /// Return all elements in self that are not in other
    pub fn diff<'a>(&'a self, other: &'a Self) -> Vec<&FilledSubshell> {
        let mut diff = Vec::new();
        for shell in &self.subshells {
            if other.subshells.contains(shell) {
                continue;
            } else {
                diff.push(shell);
            }
        }

        diff
    }

    /// Display the electron configuration in long-form format, e.g. "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d⁵"
    pub fn long_form(&self) -> String {
        use std::fmt::Write;

        let mut long_form = String::new();
        for (i, filled_subshell) in self.subshells.iter().enumerate() {
            if i > 0 {
                write!(long_form, " ").unwrap();
            }
            write!(long_form, "{}", filled_subshell).unwrap();
        }
        long_form
    }
}

impl std::iter::IntoIterator for ElectronConfiguration {
    type Item = FilledSubshell;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.subshells.into_iter()
    }
}

impl std::fmt::Display for ElectronConfiguration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut noble_notation = false;
        for (noble, noble_config) in &*NOBLE_GAS_ELECTRON_CONFIGURATIONS {
            if self.contains(noble_config) && self.subshells != noble_config.subshells {
                write!(f, "[{}] ", noble.symbol())?;

                for (i, filled_subshell) in self.diff(noble_config).iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", filled_subshell)?;
                }
                noble_notation = true;
                break;
            }
        }

        if !noble_notation {
            write!(f, "{}", self.long_form())?;
        }

        Ok(())
    }
}

impl std::str::FromStr for ElectronConfiguration {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref NAMED_ELEMENT_REGEX: Regex =
                Regex::new(r"^\[([A-Z][a-z][a-z]?)\]$").unwrap();
        }

        let mut subshells = Vec::with_capacity(s.len() / 4);

        let parts = s.split(" ");
        for part in parts {
            if let Some(caps) = NAMED_ELEMENT_REGEX.captures(part) {
                let element: Element = caps[1].parse().unwrap();
                if !element.is_noble_gas() {
                    return Err(());
                }
                let electron_config = element.electron_configuration();
                subshells.extend(electron_config.subshells);
                continue;
            }

            let subshell: FilledSubshell = part.parse().unwrap();
            subshells.push(subshell);
        }

        subshells.sort();

        if subshells.is_empty() {
            return Err(());
        }
        Ok(Self { subshells })
    }
}

impl From<&str> for ElectronConfiguration {
    fn from(s: &str) -> Self {
        s.parse().unwrap()
    }
}

impl<'de> Deserialize<'de> for ElectronConfiguration {
    fn deserialize<D>(deserializer: D) -> Result<ElectronConfiguration, D::Error>
    where
        D: Deserializer<'de>,
    {
        // Use from_str to deserialize
        let s = String::deserialize(deserializer)?;
        s.parse()
            .map_err(|_| serde::de::Error::custom("Invalid electron configuration format"))
    }
}

impl Serialize for ElectronConfiguration {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::Atom;

    use super::*;

    #[test]
    fn test_subshells() {
        let one_s: Subshell = "1s".parse().unwrap();
        assert_eq!(one_s.quantum_number(), 1);
        assert_eq!(one_s.l, 0);
        assert_eq!(one_s.electron_capacity(), 2);
        assert_eq!(one_s.num_orbitals(), 1);
        assert_eq!(one_s.shell_index(), 0);
        assert_eq!(one_s.block(), Block::S);
        assert_eq!(one_s.orbitals().len(), one_s.num_orbitals() as usize);
        assert_eq!(one_s.orbitals(), vec![Orbital::new(1, 0, 0)]);
        assert_eq!(one_s.to_string(), "1s");

        let two_s: Subshell = "2s".parse().unwrap();
        assert_eq!(two_s.quantum_number(), 2);
        assert_eq!(two_s.l, 0);
        assert_eq!(two_s.electron_capacity(), 2);
        assert_eq!(two_s.num_orbitals(), 1);
        assert_eq!(two_s.shell_index(), 1);
        assert_eq!(two_s.block(), Block::S);
        assert_eq!(two_s.orbitals().len(), two_s.num_orbitals() as usize);
        assert_eq!(two_s.orbitals(), vec![Orbital::new(2, 0, 0)]);

        let two_p: Subshell = "2p".parse().unwrap();
        assert_eq!(two_p.quantum_number(), 2);
        assert_eq!(two_p.l, 1);
        assert_eq!(two_p.electron_capacity(), 6);
        assert_eq!(two_p.num_orbitals(), 3);
        assert_eq!(two_p.shell_index(), 1);
        assert_eq!(two_p.block(), Block::P);
        assert_eq!(
            two_p.orbitals(),
            vec![
                Orbital::new(2, 1, -1),
                Orbital::new(2, 1, 0),
                Orbital::new(2, 1, 1),
            ]
        );
        assert_eq!(two_p.orbitals().len(), two_p.num_orbitals() as usize);
        assert_eq!(two_p.to_string(), "2p");
    }

    #[test]
    fn test_electron_configuration() {
        let hydrogen = Element::Hydrogen;
        assert_eq!(hydrogen.electron_configuration().to_string(), "1s¹");

        let helium = Element::Helium;
        assert_eq!(helium.electron_configuration().to_string(), "1s²");

        let lithium = Element::Lithium;
        assert_eq!(lithium.electron_configuration().to_string(), "[He] 2s¹");
        assert_eq!(lithium.electron_configuration().long_form(), "1s² 2s¹");

        let chromium = Element::Chromium;
        assert_eq!(
            chromium.electron_configuration().long_form(),
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d⁵ 4s¹"
        );
        assert_eq!(
            chromium.electron_configuration().to_string(),
            "[Ar] 3d⁵ 4s¹"
        );

        assert_eq!(
            Element::Niobium.electron_configuration().long_form(),
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d⁴ 5s¹"
        );
        assert_eq!(
            Element::Niobium.electron_configuration().to_string(),
            "[Kr] 4d⁴ 5s¹"
        );

        assert_eq!(
            Element::Gold.electron_configuration().long_form(),
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 4f¹⁴ 5s² 5p⁶ 5d¹⁰ 6s¹"
        );
        assert_eq!(
            Element::Cesium.electron_configuration().long_form(),
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 5s² 5p⁶ 6s¹"
        );
        assert_eq!(
            Element::Oganesson.electron_configuration().long_form(),
            "1s² 2s² 2p⁶ 3s² 3p⁶ 3d¹⁰ 4s² 4p⁶ 4d¹⁰ 4f¹⁴ 5s² 5p⁶ 5d¹⁰ 5f¹⁴ 6s² 6p⁶ 6d¹⁰ 7s² 7p⁶"
        );
        assert_eq!(
            Element::Oganesson.electron_configuration().to_string(),
            "[Rn] 5f¹⁴ 6d¹⁰ 7s² 7p⁶"
        );

        let oganesson_electron_config: ElectronConfiguration =
            "[Rn] 5f14 6d10 7s2 7p6".parse().unwrap();
        assert_eq!(
            Element::Oganesson.electron_configuration().to_string(),
            oganesson_electron_config.to_string()
        );

        // Test all computed configurations against known configurations in the periotic table
        for element in Element::all() {
            let computed_config = element.electron_configuration();
            let periotic_data = element.periodic_data_owned();
            let known_config = periotic_data.electron_configuration;
            assert_eq!(
                computed_config.to_string(),
                known_config.to_string(),
                "{} - should be: {}",
                element,
                known_config.long_form()
            );
        }
    }

    #[test]
    fn test_hunds_rule() {
        let nitrogen_2p = Element::Nitrogen.electron_configuration().last_subshell();
        assert_eq!(nitrogen_2p.num_electrons, 3);
        assert_eq!(nitrogen_2p.unpaired_electrons(), 3);
        assert_eq!(nitrogen_2p.num_full_orbitals(), 0);
        assert_eq!(nitrogen_2p.num_empty_orbitals(), 0);

        let oxygen_2p: FilledSubshell = Element::Oxygen.electron_configuration().last_subshell();
        assert_eq!(oxygen_2p.num_electrons, 4);
        assert_eq!(oxygen_2p.unpaired_electrons(), 2);
        assert_eq!(oxygen_2p.num_full_orbitals(), 1);
        assert_eq!(oxygen_2p.num_empty_orbitals(), 0);

        let fluorine_2p: FilledSubshell =
            Element::Fluorine.electron_configuration().last_subshell();
        assert_eq!(fluorine_2p.num_electrons, 5);
        assert_eq!(fluorine_2p.unpaired_electrons(), 1);
        assert_eq!(fluorine_2p.num_full_orbitals(), 2);
        assert_eq!(fluorine_2p.num_empty_orbitals(), 0);

        let hydrogen_1s: FilledSubshell =
            Element::Hydrogen.electron_configuration().last_subshell();
        assert_eq!(hydrogen_1s.num_electrons, 1);
        assert_eq!(hydrogen_1s.unpaired_electrons(), 1);
        assert_eq!(hydrogen_1s.num_full_orbitals(), 0);
        assert_eq!(hydrogen_1s.num_empty_orbitals(), 0);

        let proton = Atom {
            element: Element::Hydrogen,
            neutrons: None,
            electrons: 0,
        };
        let proton_1s: FilledSubshell = proton.electron_configuration().last_subshell();
        assert_eq!(proton_1s.num_electrons, 0);
        assert_eq!(proton_1s.unpaired_electrons(), 0);
        assert_eq!(proton_1s.num_full_orbitals(), 0);
        assert_eq!(proton_1s.num_empty_orbitals(), 1);
        assert_eq!(proton_1s.to_string(), "1s⁰");
        assert_eq!(proton_1s.is_empty(), true);
    }
}
