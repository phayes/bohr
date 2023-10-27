use crate::{util, Atom};

#[derive(Debug, Clone, PartialEq, Eq)]
enum SimpleMoleculePart {
    Atom(Atom),
    CompoundPart(Box<SimpleMolecule>),
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

        let molecule = SimpleMoleculePart::CompoundPart(Box::new(molecule));
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
    /// Place all Compound Parts at the end
    pub fn sort(&mut self) {
        let contains_carbon = self.parts.iter().any(|(part, _)| match part {
            SimpleMoleculePart::Atom(atom) => atom.element == crate::Element::Carbon,
            SimpleMoleculePart::CompoundPart(_) => false,
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
                (SimpleMoleculePart::CompoundPart(_), SimpleMoleculePart::Atom(_)) => {
                    std::cmp::Ordering::Greater
                }
                (SimpleMoleculePart::Atom(_), SimpleMoleculePart::CompoundPart(_)) => {
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
            SimpleMoleculePart::CompoundPart(molecule) => {
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

        // Organic Molecule
        let mut molecule = SimpleMolecule::default();
        molecule.add_atom(Element::Carbon.atom(), 1);
        molecule.add_atom(Element::Hydrogen.atom(), 4);
        molecule.add_atom(Element::Oxygen.atom(), 2);
        molecule.add_molecule(molecule.clone(), 1);
        molecule.sort();
        assert_eq!(molecule.to_string(), "CH₄O₂(CH₄O₂)");
    }
}