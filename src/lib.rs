mod element;
pub use element::*;
pub mod periodictable;
pub(crate) mod util;
// pub mod reactant;
pub mod orbitals;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
