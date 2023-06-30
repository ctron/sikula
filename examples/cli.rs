include!("../tests/resource.rs");

fn main() {
    let query = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    println!("Query: '{query}'");
    let query = DeriveResource::parse(&query).unwrap();
    println!("Parsed:");
    println!("{query:#?}");
}
