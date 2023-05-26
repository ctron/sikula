use crate::hir;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct Qualifier<'a>(pub Vec<&'a str>);

impl<'a> Display for Qualifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, s) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str(":")?;
            }
            f.write_str(s)?;
        }
        Ok(())
    }
}

impl<'a> Deref for Qualifier<'a> {
    type Target = Vec<&'a str>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Qualifier<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Qualifier<'_> {
    pub const fn empty() -> Self {
        Self(vec![])
    }
}

impl<'a> FromIterator<&'a str> for Qualifier<'a> {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Qualifier(Vec::from_iter(iter))
    }
}

impl<'a> From<&[&'a str]> for Qualifier<'a> {
    fn from(value: &[&'a str]) -> Self {
        Self(value.into())
    }
}

impl<'a, const N: usize> From<[&'a str; N]> for Qualifier<'a> {
    fn from(value: [&'a str; N]) -> Self {
        Self(value.into())
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub enum Direction {
    #[default]
    Ascending,
    Descending,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Sort<'a> {
    pub qualifier: Qualifier<'a>,
    pub direction: Direction,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'a> {
    Predicate,
    Simple(&'a str),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Term<'a> {
    pub qualifier: Qualifier<'a>,
    pub invert: bool,
    pub expression: Expression<'a>,
}

impl<'a> Term<'a> {
    pub fn primary(value: &'a str) -> Self {
        Self {
            qualifier: Qualifier::empty(),
            invert: false,
            expression: Expression::Simple(value),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Query<'a> {
    pub terms: Vec<Term<'a>>,
    pub scope: Vec<Qualifier<'a>>,
    pub sorting: Vec<Sort<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {}

impl<'a> Query<'a> {
    pub fn parse(query: hir::Query<'a>) -> Result<Self, Error> {
        let mut terms = Vec::new();
        let mut scope = Vec::new();
        let mut sorting = Vec::new();

        for term in query.terms {
            match term.tokens.as_slice() {
                [] => {
                    // should not happen, skip
                }
                ["in", qualifier @ ..] => scope.push(qualifier.into()),
                ["is", qualifier @ ..] => terms.push(Term {
                    qualifier: qualifier.into(),
                    invert: term.invert,
                    expression: Expression::Predicate,
                }),
                ["sort", qualifier @ ..] => {
                    sorting.push(Sort {
                        qualifier: qualifier.into(),
                        direction: match term.invert {
                            false => Direction::Ascending,
                            true => Direction::Descending,
                        },
                    });
                }
                [primary] => terms.push(Term::primary(primary)),
                [qualifier @ .., value] => terms.push(Term {
                    qualifier: qualifier.into(),
                    invert: term.invert,
                    expression: Expression::Simple(value),
                }),
            }
        }

        Ok(Self {
            terms,
            scope,
            sorting,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parser;
    use chumsky::Parser;

    #[test]
    fn test() {
        let result = Query::parse(
            parser()
                .parse(r#"is:predicate primary -qualifier:foo in:scope sort:something"#)
                .unwrap(),
        )
        .unwrap();

        assert_eq!(
            Query {
                terms: vec![
                    Term {
                        qualifier: ["predicate"].into(),
                        invert: false,
                        expression: Expression::Predicate,
                    },
                    Term {
                        qualifier: [].into(),
                        invert: false,
                        expression: Expression::Simple("primary"),
                    },
                    Term {
                        qualifier: ["qualifier"].into(),
                        invert: true,
                        expression: Expression::Simple("foo")
                    }
                ],
                scope: vec![["scope"].into()],
                sorting: vec![Sort {
                    qualifier: ["something"].into(),
                    direction: Direction::Ascending,
                }],
            },
            result
        );
    }

    #[test]
    fn test_multi_scope() {
        let result = Query::parse(parser().parse(r#"in:a in:b in:c"#).unwrap()).unwrap();

        assert_eq!(
            Query {
                terms: vec![],
                scope: vec![["a"].into(), ["b"].into(), ["c"].into()],
                sorting: vec![],
            },
            result
        );
    }

    #[test]
    fn test_multi_sort() {
        let result = Query::parse(parser().parse(r#"sort:foo -sort:bar"#).unwrap()).unwrap();

        assert_eq!(
            Query {
                terms: vec![],
                scope: vec![],
                sorting: vec![
                    Sort {
                        qualifier: ["foo"].into(),
                        direction: Direction::Ascending
                    },
                    Sort {
                        qualifier: ["bar"].into(),
                        direction: Direction::Descending
                    }
                ],
            },
            result
        );
    }

    #[test]
    fn test_primaries() {
        let result = Query::parse(parser().parse(r#"foo "bar, baz""#).unwrap()).unwrap();

        assert_eq!(
            Query {
                terms: vec![Term::primary("foo"), Term::primary("bar, baz"),],
                scope: vec![],
                sorting: vec![],
            },
            result
        );
    }
}
