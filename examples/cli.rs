include!("../tests/resource.rs");

fn main() {
    let query = std::env::args().skip(1).collect::<Vec<_>>().join(" ");
    println!("Input: '{query}'");
    let query = DeriveResource::parse(&query).unwrap();
    println!("{query:#?}");
}
