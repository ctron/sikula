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
pub enum Term<'a> {
    Match {
        qualifier: Qualifier<'a>,
        expression: Expression<'a>,
    },
    Not(Box<Term<'a>>),
    And {
        scopes: Vec<Qualifier<'a>>,
        terms: Vec<Term<'a>>,
    },
    Or {
        scopes: Vec<Qualifier<'a>>,
        terms: Vec<Term<'a>>,
    },
}

impl<'a> Term<'a> {
    /*
    pub fn primary(value: &'a str) -> Self {
        Self {
            qualifier: Qualifier::empty(),
            invert: false,
            expression: Expression::Simple(value),
        }
    }*/

    pub fn primary(value: &'a str) -> Self {
        Self::Match {
            qualifier: Qualifier::empty(),
            expression: Expression::Simple(value),
        }
    }

    pub fn compact(self) -> Self {
        match self {
            // if "or" has only one element an no scopes, return the single term
            Self::Or { scopes, mut terms } if scopes.is_empty() && terms.len() == 1 => {
                terms.remove(0)
            }
            // if "and" has only one element an no scopes, return the single term
            Self::And { scopes, mut terms } if scopes.is_empty() && terms.len() == 1 => {
                terms.remove(0)
            }
            otherwise => otherwise,
        }
    }

    pub fn negate(self) -> Self {
        match self {
            // if we are already applying "not", then negating just means removing that not
            Self::Not(term) => *term,
            // otherwise, wrap with a not
            otherwise => Self::Not(Box::new(otherwise)),
        }
    }

    fn apply_invert(self, invert: bool) -> Self {
        if invert {
            self.negate()
        } else {
            self
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Query<'a> {
    pub term: Term<'a>,
    pub sorting: Vec<Sort<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {}

impl<'a> Query<'a> {
    pub fn parse(query: hir::Query<'a>) -> Result<Self, Error> {
        let mut sorting = Vec::new();

        // the root, defaults to "and"
        let mut scopes = vec![];
        let terms = Self::parse_term(&mut sorting, &mut scopes, &query.term);

        Ok(Self {
            term: Term::And { terms, scopes }.compact(),
            sorting,
        })
    }

    fn parse_term(
        sorting: &mut Vec<Sort<'a>>,
        scopes: &mut Vec<Qualifier<'a>>,
        term: &hir::Term<'a>,
    ) -> Vec<Term<'a>> {
        match term {
            hir::Term::Not(term) => Self::parse_term(sorting, scopes, &term)
                .into_iter()
                .map(|term| Term::Not(Box::new(term)))
                .collect(),
            hir::Term::And(terms) => {
                let mut scopes = vec![];
                let terms = terms
                    .iter()
                    .flat_map(|term| Self::parse_term(sorting, &mut scopes, term))
                    .collect();
                vec![Term::And { scopes, terms }.compact()]
            }
            hir::Term::Or(terms) => {
                let mut scopes = vec![];
                let terms = terms
                    .iter()
                    .flat_map(|term| Self::parse_term(sorting, &mut scopes, term))
                    .collect();
                vec![Term::Or { scopes, terms }.compact()]
            }
            hir::Term::Match { invert, tokens } => {
                Self::parse_match(*invert, tokens, sorting, scopes)
            }
        }
    }

    fn parse_match(
        invert: bool,
        terms: &Vec<&'a str>,
        sorting: &mut Vec<Sort<'a>>,
        scopes: &mut Vec<Qualifier<'a>>,
    ) -> Vec<Term<'a>> {
        match terms.as_slice() {
            [] => {
                // should not happen, skip
                vec![]
            }
            ["in", qualifier @ ..] => {
                scopes.push(qualifier.into());
                vec![]
            }
            ["is", qualifier @ ..] => vec![Term::Match {
                qualifier: qualifier.into(),
                expression: Expression::Predicate,
            }
            .apply_invert(invert)],
            ["sort", qualifier @ ..] => {
                sorting.push(Sort {
                    qualifier: qualifier.into(),
                    direction: match invert {
                        false => Direction::Ascending,
                        true => Direction::Descending,
                    },
                });
                vec![]
            }
            [primary] => vec![Term::primary(primary).apply_invert(invert)],
            [qualifier @ .., value] => vec![Term::Match {
                qualifier: qualifier.into(),
                expression: Expression::Simple(value),
            }
            .apply_invert(invert)],
        }
    }

    /*
    pub fn parseX(query: hir::Query<'a>) -> Result<Self, Error> {
        let mut terms = Vec::new();
        let mut sorting = Vec::new();

        for term in query.term {
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
    }*/
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
                term: Term::And {
                    terms: vec![
                        Term::Match {
                            qualifier: ["predicate"].into(),
                            expression: Expression::Predicate,
                        },
                        Term::Match {
                            qualifier: Qualifier::default(),
                            expression: Expression::Simple("primary"),
                        },
                        Term::Not(Box::new(Term::Match {
                            qualifier: ["qualifier"].into(),
                            expression: Expression::Simple("foo")
                        }))
                    ],
                    scopes: vec![["scope"].into()]
                },

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
                term: Term::And {
                    terms: vec![],
                    scopes: vec![["a"].into(), ["b"].into(), ["c"].into()],
                },
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
                term: Term::And {
                    scopes: vec![],
                    terms: vec![]
                },
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
                term: Term::And {
                    scopes: vec![],
                    terms: vec![Term::primary("foo"), Term::primary("bar, baz")]
                },
                sorting: vec![],
            },
            result
        );
    }
}
