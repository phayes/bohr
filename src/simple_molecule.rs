/// A simple moledule is a molecule that can be represented as a simple formula and does not need a more complex representation.
enum SimpleMoleculePartType {
    Atom(Atom),
    CompoundPart(Box<Vec<SimpleMoleculePart>>),
}

struct SimpleMoleculePart {
    part: FormulaPartType,
    count: u32,
}

pub struct SimpleMolecule {
    parts: Vec<FormulaPart>,
    charge_number: i32,

    // Optional fields that may be undefined and can be used to further describe the molecule
    // TODO: Do these even belong here?
    coefficient: Option<u32>,
    state: Option<State>,
}

pub enum State {
    Solid,
    Liquid,
    Gas,
    Aqueous,
}
