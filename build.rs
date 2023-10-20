use serde::{Deserialize, Serialize};
use serde_big_array::BigArray;

fn main() {

    #[derive(Serialize, Deserialize)]
    pub struct PeriodicElement<'a> {
        name: &'a str,
        appearance: Option<&'a str>,
        atomic_mass: f64,
        boil: Option<f64>,
        category: &'a str,
        density: Option<f64>,
        discovered_by: Option<&'a str>,
        melt: Option<f64>,
        molar_heat: Option<f64>,
        named_by: Option<&'a str>,
        number: u32,
        period: u32,
        group: u32,
        phase: &'a str,
        source: &'a str,
        bohr_model_image: Option<&'a str>,
        bohr_model_3d: Option<&'a str>,
        spectral_img: Option<&'a str>,
        summary: &'a str,
        symbol: &'a str,
        xpos: u32,
        ypos: u32,
        wxpos: u32,
        wypos: u32,
        shells: Vec<u32>,
        electron_configuration: &'a str,
        electron_configuration_semantic: &'a str,
        electron_affinity: Option<f64>,
        electronegativity_pauling: Option<f64>,
        ionization_energies: Vec<f64>,
        cpk_hex: Option<&'a str>,
        image: PeriodicImage<'a>,
        block: &'a str,
    }
    
    #[derive(Serialize, Deserialize)]
    pub struct PeriodicImage<'a> {
        title: &'a str,
        url: &'a str,
        attribution: &'a str,
    }

    #[derive(Serialize, Deserialize)]
    pub struct PeriodicTable<'a> {
        #[serde(with = "BigArray")]
        #[serde(borrow = "'a")]
        elements: [PeriodicElement<'a>; 119],
    }

    let table: PeriodicTable = serde_json::from_str(include_str!("data/periodic_table.json")).unwrap();
    uneval::to_out_dir(table, "periodic_table_data.rs").unwrap();
}