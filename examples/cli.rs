use sikula::lir;

include!("../tests/resource.rs");

fn main() {
    let query = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    println!("Input: '{query}'");

    let query = lir::parse_query(&query).expect("Failed to parse into MIR");
    println!("\nMIR:\n{query:#?}");
    let query = DeriveResource::parse_from(query).expect("Failed to parse into LIR");
    println!("\nLIR:\n{query:#?}");
}
