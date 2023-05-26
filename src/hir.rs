#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'a> {
    pub invert: bool,
    pub tokens: Vec<&'a str>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Query<'a> {
    pub terms: Vec<Term<'a>>,
}
