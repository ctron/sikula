use crate::hir::{Query, Term};
use chumsky::prelude::*;

pub fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> {
    any()
        .filter(|c: &char| c.is_alphanumeric())
        .repeated()
        .at_least(1)
        .map_slice(|s| s)
}

pub fn quoted_string<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> {
    just('"')
        .ignore_then(none_of('"').repeated().slice())
        .then_ignore(just('"'))
}

pub fn expression_string<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> {
    any()
        .filter(|c: &char| !(c.is_whitespace() || c.is_control() || c == &'"'))
        .repeated()
        .at_least(1)
        .map_slice(|s| s)
}

pub fn value<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> {
    quoted_string().or(expression_string())
}

pub fn term<'a>() -> impl Parser<'a, &'a str, Term<'a>, extra::Err<Simple<'a, char>>> {
    just('-')
        .repeated()
        .at_most(1)
        .count()
        .then(
            ident()
                .then_ignore(just(':'))
                .repeated()
                .collect::<Vec<_>>()
                .then(value())
                .map(|(mut tokens, value)| {
                    tokens.push(value);
                    tokens
                }),
        )
        .map(|(invert, tokens)| Term {
            invert: invert > 0,
            tokens,
        })
}

pub fn parser<'a>() -> impl Parser<'a, &'a str, Query<'a>, extra::Err<Simple<'a, char>>> {
    term()
        .padded()
        .repeated()
        .collect()
        .map(|terms| Query { terms })
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_parse(query: &str, expected: Query) {
        let result = parser().parse(query).unwrap();

        assert_eq!(result, expected);
    }

    fn assert_parse_err(query: &str) {
        assert!(parser().parse(query).has_errors());
    }

    #[test]
    fn test_empty() {
        assert_parse("", Query { terms: vec![] })
    }

    #[test]
    fn test_one() {
        assert_parse(
            "is:predicate",
            Query {
                terms: vec![Term {
                    tokens: vec!["is", "predicate"],
                    invert: false,
                }],
            },
        )
    }

    #[test]
    fn test_two() {
        assert_parse(
            "is:predicate foo:bar",
            Query {
                terms: vec![
                    Term {
                        tokens: vec!["is", "predicate"],
                        invert: false,
                    },
                    Term {
                        tokens: vec!["foo", "bar"],
                        invert: false,
                    },
                ],
            },
        )
    }

    #[test]
    fn test_with_primary() {
        assert_parse(
            "foo is:predicate bar",
            Query {
                terms: vec![
                    Term {
                        tokens: vec!["foo"],
                        invert: false,
                    },
                    Term {
                        tokens: vec!["is", "predicate"],
                        invert: false,
                    },
                    Term {
                        tokens: vec!["bar"],
                        invert: false,
                    },
                ],
            },
        )
    }

    #[test]
    fn test_with_quotes() {
        assert_parse(
            r#"foo:"is bar" is:predicate bar"#,
            Query {
                terms: vec![
                    Term {
                        tokens: vec!["foo", "is bar"],
                        invert: false,
                    },
                    Term {
                        tokens: vec!["is", "predicate"],
                        invert: false,
                    },
                    Term {
                        tokens: vec!["bar"],
                        invert: false,
                    },
                ],
            },
        )
    }

    #[test]
    fn test_with_no_value() {
        assert_parse_err("foo:");
    }

    #[test]
    fn test_with_no_value_quoted() {
        assert_parse(
            r#"foo:"""#,
            Query {
                terms: vec![Term {
                    tokens: vec!["foo", ""],
                    invert: false,
                }],
            },
        )
    }

    #[test]
    fn test_invert() {
        assert_parse(
            r#"-is:something is:something"#,
            Query {
                terms: vec![
                    Term {
                        tokens: vec!["is", "something"],
                        invert: true,
                    },
                    Term {
                        tokens: vec!["is", "something"],
                        invert: false,
                    },
                ],
            },
        )
    }

    #[test]
    fn test_range() {
        assert_parse(
            r#"size:*..*"#,
            Query {
                terms: vec![Term {
                    tokens: vec!["size", "*..*"],
                    invert: false,
                }],
            },
        )
    }
}
