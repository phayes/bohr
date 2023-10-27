use crate::{util, Atom};
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq)]
enum SimpleMoleculePart {
    Atom(Atom),
    Compound(Box<SimpleMolecule>),
}

/// A simple moledule is a molecule that can be represented as a simple formula and does not need a more complex representation.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SimpleMolecule {
    parts: Vec<(SimpleMoleculePart, u8)>,
    charge_number: i8,
}

impl SimpleMolecule {
    pub fn add_atom(&mut self, atom: Atom, count: u8) {
        // Remove charge from atom and add to molecule
        let mut atom = atom;
        self.charge_number += atom.charge_number();
        atom.electrons = atom.element.atomic_number();

        let atom = SimpleMoleculePart::Atom(atom);
        let mut added = false;
        self.parts.iter_mut().for_each(|(part, c)| {
            if part == &atom {
                *c += count;
                added = true;
            }
        });

        if !added {
            self.parts.push((atom, count));
        }
    }

    pub fn add_molecule(&mut self, molecule: SimpleMolecule, count: u8) {
        let mut molecule = molecule;
        self.charge_number += molecule.charge_number;
        molecule.charge_number = 0;

        let molecule = SimpleMoleculePart::Compound(Box::new(molecule));
        let mut added = false;
        self.parts.iter_mut().for_each(|(part, c)| {
            if part == &molecule {
                *c += count;
                added = true;
            }
        });
        if !added {
            self.parts.push((molecule, count));
        }
    }

    /// For molecules that contain carbon, the order is carbon, hydrogen, then all other elements in alphabetical order of their chemical symbols.
    /// If the structure does not contain carbon, write all elements, including hydrogen, in alphabetical order of their chemical symbols.
    /// Place all Compound parts at the end
    pub fn sort(&mut self) {
        let contains_carbon = self.parts.iter().any(|(part, _)| match part {
            SimpleMoleculePart::Atom(atom) => atom.element == crate::Element::Carbon,
            SimpleMoleculePart::Compound(_) => false,
        });

        self.parts
            .sort_by(|(part_a, _), (part_b, _)| match (part_a, part_b) {
                (SimpleMoleculePart::Atom(atom_a), SimpleMoleculePart::Atom(atom_b)) => {
                    if contains_carbon {
                        if atom_a.element == crate::Element::Carbon {
                            std::cmp::Ordering::Less
                        } else if atom_b.element == crate::Element::Carbon {
                            std::cmp::Ordering::Greater
                        } else if atom_a.element == crate::Element::Hydrogen {
                            std::cmp::Ordering::Less
                        } else if atom_b.element == crate::Element::Hydrogen {
                            std::cmp::Ordering::Greater
                        } else {
                            atom_a.element.symbol().cmp(&atom_b.element.symbol())
                        }
                    } else {
                        atom_a.element.symbol().cmp(&atom_b.element.symbol())
                    }
                }
                (SimpleMoleculePart::Compound(_), SimpleMoleculePart::Atom(_)) => {
                    std::cmp::Ordering::Greater
                }
                (SimpleMoleculePart::Atom(_), SimpleMoleculePart::Compound(_)) => {
                    std::cmp::Ordering::Less
                }
                _ => std::cmp::Ordering::Equal,
            });
    }
}

impl std::fmt::Display for SimpleMolecule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut result = String::with_capacity(self.parts.len() * 3);
        self.parts.iter().for_each(|(part, c)| match part {
            SimpleMoleculePart::Atom(atom) => {
                let count = if *c == 1 {
                    "".to_string()
                } else {
                    util::subscript(c.to_string())
                };
                result.push_str(&format!("{}{}", atom, count));
            }
            SimpleMoleculePart::Compound(molecule) => {
                let count = if *c == 1 {
                    "".to_string()
                } else {
                    util::subscript(c.to_string())
                };
                result.push_str(&format!("({}){}", molecule, count));
            }
        });

        if self.charge_number > 0 {
            result.push_str(&format!(
                "⁺{}",
                util::superscript_number(self.charge_number as u8)
            ));
        } else if self.charge_number < 0 {
            result.push_str(&format!(
                "⁻{}",
                util::superscript_number(self.charge_number.abs() as u8)
            ));
        }

        write!(f, "{}", result)
    }
}

impl std::str::FromStr for SimpleMolecule {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref SIMPLE_MOLECULE_REGEX: Regex = Regex::new(r"(((?<atom>([⁰¹²³⁴⁵⁶⁷⁸⁹]+)?([A-Z][a-z]?))(?<atom_count>[₀₁₂₃₄₅₆₇₈₉0-9]+)?)|(\((?<compound>.+)\)(?<compound_count>[₀₁₂₃₄₅₆₇₈₉0-9]+)?))").unwrap();
        }

        let mut molecule = SimpleMolecule::default();

        let parts: Vec<&str> = SIMPLE_MOLECULE_REGEX.find_iter(s).map(|m| m.as_str()).collect();

        for part in parts {
            let caps = SIMPLE_MOLECULE_REGEX.captures(part).unwrap();
            if let Some(atom) = caps.name("atom") {
                let atom: Atom = atom.as_str().parse()?;
                let count = match caps.name("atom_count") {
                    Some(count) => util::regular_case(count.as_str()).parse().map_err(|_| ())?,
                    None => 1,
                };
                molecule.add_atom(atom, count);
            }
            else if let Some(compound) = caps.name("compound") {
                let compound: SimpleMolecule = compound.as_str().parse()?;
                let count = match caps.name("compound_count") {
                    Some(count) => util::regular_case(count.as_str()).parse().map_err(|_| ())?,
                    None => 1,
                };
                molecule.add_molecule(compound, count);
            }
        }

        // TODO Capture charge
        //      Add charge as another OR statement in regex that ends with a $ terminator

        molecule.sort();

        Ok(molecule)
    }
}

#[cfg(test)]

mod tests {
    use super::*;
    use crate::Atom;
    use crate::Element;

    #[test]
    fn test_simple_molecule() {
        // Water
        let mut molecule = SimpleMolecule::default();
        molecule.add_atom(Element::Hydrogen.atom(), 2);
        molecule.add_atom(Element::Oxygen.atom(), 1);
        molecule.sort();
        assert_eq!(molecule.to_string(), "H₂O");
        assert_eq!("H₂O".parse::<SimpleMolecule>().unwrap(), molecule);

        // Heavy Water
        let mut molecule = SimpleMolecule::default();
        molecule.add_atom(Atom {
            element: Element::Hydrogen,
            electrons: 1,
            neutrons: Some(1),
        }, 2);
        molecule.add_atom(Element::Oxygen.atom(), 1);
        molecule.sort();
        assert_eq!(molecule.to_string(), "²H₂O");
        assert_eq!("²H₂O".parse::<SimpleMolecule>().unwrap(), molecule);

        // Organic Molecule
        let mut molecule = SimpleMolecule::default();
        molecule.add_atom(Element::Carbon.atom(), 1);
        molecule.add_atom(Element::Hydrogen.atom(), 4);
        molecule.add_atom(Element::Oxygen.atom(), 2);
        molecule.add_molecule(molecule.clone(), 1);
        molecule.sort();
        assert_eq!(molecule.to_string(), "CH₄O₂(CH₄O₂)");
        assert_eq!("CH₄O₂(CH₄O₂)".parse::<SimpleMolecule>().unwrap(), molecule);
    }
}