use crate::hir::{Query, Term};
use chumsky::prelude::*;

pub fn ident<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> + Clone {
    any()
        .filter(|c: &char| c.is_alphanumeric())
        .repeated()
        .at_least(1)
        .map_slice(|s: &'a str| s)
}

pub fn quoted_string<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> + Clone
{
    just('"')
        .ignore_then(none_of('"').repeated().slice())
        .then_ignore(just('"'))
}

pub fn expression_string<'a>(
) -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> + Clone {
    any()
        .filter(|c: &char| {
            c.is_alphanumeric()
                || match *c {
                    // comparison
                    '<' | '>' | '=' => true,
                    // range
                    '.' | '*' => true,
                    // date
                    '-' => true,
                    _ => false,
                }
        })
        .repeated()
        .at_least(1)
        .map_slice(|s| s)
        // filter out keywords in non-escaped values
        .filter(|ident| !matches!(*ident, "OR" | "AND" | "NOT"))
}

pub fn value<'a>() -> impl Parser<'a, &'a str, &'a str, extra::Err<Simple<'a, char>>> + Clone {
    quoted_string().or(expression_string())
}

pub fn values<'a>() -> impl Parser<'a, &'a str, Vec<&'a str>, extra::Err<Simple<'a, char>>> + Clone
{
    value()
        .separated_by(just(','))
        .at_least(1)
        .allow_leading()
        .allow_trailing()
        .collect()
}

pub fn term_match<'a>() -> impl Parser<'a, &'a str, Term<'a>, extra::Err<Simple<'a, char>>> + Clone
{
    let invert = just('-').repeated().at_most(1).count();

    invert
        .then(
            ident()
                .then_ignore(just(':'))
                .repeated()
                .collect::<Vec<_>>()
                .then(values()),
        )
        .map(|(invert, (tokens, values))| {
            // check inversion
            let invert = invert > 0;

            // expand multi value case
            let terms = values.into_iter().map(|value| {
                // take base tokens and add value
                let tokens = tokens.iter().copied().chain([value]).collect();
                Term::Match { invert, tokens }
            });

            // multi value as "or" (or single)
            Term::or(terms)
        })
}

/// Create a new parser for parsing a query.
pub fn parser<'a>() -> impl Parser<'a, &'a str, Query<'a>, extra::Err<Simple<'a, char>>> {
    recursive(|expr| {
        let term = term_match();

        let op = |c| just(c).padded();

        let atom = term
            .or(expr.clone().delimited_by(just('('), just(')')))
            .padded();

        let not = op("NOT")
            .repeated()
            .foldr(atom, |_op, rhs| Term::Not(Box::new(rhs)));

        let and = not
            .clone()
            .foldl(op("AND").ignore_then(not.clone()).repeated(), |lhs, rhs| {
                Term::binary(Term::And, lhs, rhs)
            });

        let or = and
            .clone()
            .foldl(op("OR").ignore_then(and).repeated(), |lhs, rhs| {
                Term::binary(Term::Or, lhs, rhs)
            });

        or.repeated().collect::<Vec<_>>().map(Term::and)
    })
    .padded()
    .map(|term| Query {
        // turn into a query, compacting the tree
        term: term.compact(),
    })
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
        assert_parse(
            "",
            Query {
                term: Term::And(vec![]),
            },
        )
    }

    #[test]
    fn test_one() {
        assert_parse(
            "is:predicate",
            Query {
                term: Term::Match {
                    invert: false,
                    tokens: vec!["is", "predicate"],
                },
            },
        )
    }

    #[test]
    fn test_two() {
        assert_parse(
            "is:predicate foo:bar",
            Query {
                term: Term::And(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["is", "predicate"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "bar"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_with_primary() {
        assert_parse(
            "foo is:predicate bar",
            Query {
                term: Term::And(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["is", "predicate"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["bar"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_with_primaries() {
        assert_parse(
            "foo bar",
            Query {
                term: Term::And(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["bar"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_with_quotes() {
        assert_parse(
            r#"foo:"is bar" is:predicate bar"#,
            Query {
                term: Term::And(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "is bar"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["is", "predicate"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["bar"],
                    },
                ]),
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
                term: Term::Match {
                    invert: false,
                    tokens: vec!["foo", ""],
                },
            },
        )
    }

    #[test]
    fn test_qualifier_multi() {
        assert_parse(
            r#"foo:a,b,c"#,
            Query {
                term: Term::Or(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "a"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "b"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "c"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_qualifier_multi_quote() {
        assert_parse(
            r#"foo:a,"foo () bar",c"#,
            Query {
                term: Term::Or(vec![
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "a"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "foo () bar"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["foo", "c"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_invert() {
        assert_parse(
            r#"-is:something is:something"#,
            Query {
                term: Term::And(vec![
                    Term::Match {
                        invert: true,
                        tokens: vec!["is", "something"],
                    },
                    Term::Match {
                        invert: false,
                        tokens: vec!["is", "something"],
                    },
                ]),
            },
        )
    }

    #[test]
    fn test_range() {
        assert_parse(
            r#"size:*..*"#,
            Query {
                term: Term::Match {
                    invert: false,
                    tokens: vec!["size", "*..*"],
                },
            },
        )
    }

    #[test]
    fn test_date() {
        assert_parse(
            r#"date:2022-01-01"#,
            Query {
                term: Term::Match {
                    invert: false,
                    tokens: vec!["date", "2022-01-01"],
                },
            },
        );
    }

    #[test]
    fn test_not() {
        assert_parse(
            r#"NOT A"#,
            Query {
                term: Term::Not(Box::new(Term::r#match(["A"]))),
            },
        );
    }

    #[test]
    fn test_and_basic() {
        assert_parse(
            r#"A B C"#,
            Query {
                term: Term::and([
                    Term::r#match(["A"]),
                    Term::r#match(["B"]),
                    Term::r#match(["C"]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_simple() {
        assert_parse(
            r#"A AND B"#,
            Query {
                term: Term::and([Term::r#match(["A"]), Term::r#match(["B"])]),
            },
        );
    }

    #[test]
    fn test_and_mixed() {
        assert_parse(
            r#"A B C AND D"#,
            Query {
                term: Term::and([
                    Term::r#match(["A"]),
                    Term::r#match(["B"]),
                    Term::r#match(["C"]),
                    Term::r#match(["D"]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_mixed_2() {
        assert_parse(
            r#"A B OR C"#,
            Query {
                term: Term::and([
                    Term::r#match(["A"]),
                    Term::or([Term::r#match(["B"]), Term::r#match(["C"])]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_mixed_3() {
        assert_parse(
            r#"A OR B AND C"#,
            Query {
                term: Term::or([
                    Term::r#match(["A"]),
                    Term::and([Term::r#match(["B"]), Term::r#match(["C"])]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_mixed_4() {
        assert_parse(
            r#"A OR B C"#,
            Query {
                term: Term::and([
                    Term::or([Term::r#match(["A"]), Term::r#match(["B"])]),
                    Term::r#match(["C"]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_or_1() {
        assert_parse(
            r#"A AND B AND C OR D"#,
            Query {
                term: Term::or([
                    Term::and([
                        Term::r#match(["A"]),
                        Term::r#match(["B"]),
                        Term::r#match(["C"]),
                    ]),
                    Term::r#match(["D"]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_or_2() {
        assert_parse(
            r#"A B C OR D"#,
            Query {
                term: Term::and([
                    Term::r#match(["A"]),
                    Term::r#match(["B"]),
                    Term::or([Term::r#match(["C"]), Term::r#match(["D"])]),
                ]),
            },
        );
    }

    #[test]
    fn test_and_or_3() {
        assert_parse(
            r#"( A B C ) OR D"#,
            Query {
                term: Term::or([
                    Term::and([
                        Term::r#match(["A"]),
                        Term::r#match(["B"]),
                        Term::r#match(["C"]),
                    ]),
                    Term::r#match(["D"]),
                ]),
            },
        );
    }
}
