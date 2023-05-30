use sikula::prelude::*;

#[derive(Search)]
pub enum TestA<'a> {
    Foo(&'a str),
}
