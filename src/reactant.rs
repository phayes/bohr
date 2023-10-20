use crate::element::*;

pub enum Reactant {
    Atom(Atom),
    Element(Element),
    Compound(MolecularFormula),
    Energy(f64),
}
enum FormulaPartType {
    Atom(Atom),
    CompoundPart(Box<Vec<FormulaPart>>),
}

struct FormulaPart {
    part: FormulaPartType,
    count: u32,
}

pub struct MolecularFormula {
    parts: Vec<FormulaPart>,
    charge_number: i32,

    // Optional fields that may be undefined
    coefficient: Option<u32>,
    state: Option<State>,
    product_evolution: Option<ProductEvolution>,
}

impl MolecularFormula {
    // pub fn vsper(&self) -> Chemical {
    // 
    // }
}

pub enum ProductEvolution {
    Liberation,
    Precipitation,
}

pub enum State {
    Solid,
    Liquid,
    Gas,
    Aqueous,
    Plasma,
}
