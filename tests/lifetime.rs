use sikula_macros::Search;

include!("resource.rs");

fn use_example<S>()
where
    S: Search,
{
    let _ok = match S::parse("") {
        Ok(_) => true,
        Err(_) => false,
    };
}

#[test]
fn test_lifetimes() {
    use_example::<DeriveResource>();
}
