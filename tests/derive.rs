use sikula::prelude::*;

/// same as [`ExampleResource`], but manually implemented
#[derive(Search, Clone, Debug, PartialEq, Eq)]
enum DeriveResource<'a> {
    /// Standard qualifier: `author:someone`
    #[search(sort, scope)]
    Author(&'a str),
    /// Default primary: `warranty`
    #[search(default)]
    Subject(Primary<'a>),
    /// Non-default primary: `warranty in:message`, to search in both: `warranty in:message in:subject`
    #[search(scope)]
    Message(Primary<'a>),

    /// Predicate: `is:read`
    Read,

    /// Numeric qualifier example:
    /// * `size:100` (equals)
    /// * `size:>=100` (size greater than or equals 100)
    /// * `size:100..200` (size between 100 inclusive and 200 exclusive)
    /// * `size:*..200` (size up to 200 exclusive)
    #[search(sort)]
    Size(Ordered<usize>),

    #[search(sort)]
    Sent(Ordered<time::OffsetDateTime>),

    Label(Qualified<'a, &'a str>),
}

/*
#[derive(Resource)]
enum ExampleResource<'a> {
    /// Standard qualifier: `author:someone`
    #[query(sort)]
    Author(&'a str),
    /// Default primary: `warranty`
    #[query(primary, default = true)]
    Subject(&'a str),
    /// Non-default primary: `warranty in:message`, to search in both: `warranty in:message in:subject`
    #[query(primary)]
    Message(&'a str),

    /// Predicate: `is:read`
    Read,

    /// Numeric qualifier example:
    /// * `size:100` (equals)
    /// * `size:>=100` (size greater than or equals 100)
    /// * `size:100..200` (size between 100 inclusive and 200 exclusive)
    /// * `size:*..200` (size up to 200 exclusive)
    #[query(sort)]
    Size(Ordered<usize>),

    #[query(sort)]
    Sent(Ordered<time::OffsetDateTime>),

    #[query]
    Label(Qualified<'a, String>)
}*/

#[test]
fn test() {
    let r = DeriveResource::parse(r#"is:read message:bar -subject:foo"#).unwrap();

    assert_eq!(
        Query {
            term: Term::And(vec![
                Term::Match(DeriveResource::Read),
                Term::Match(DeriveResource::Message(Primary::Equal("bar"))),
                Term::Not(Box::new(Term::Match(DeriveResource::Subject(
                    Primary::Equal("foo")
                ))))
            ]),
            sorting: vec![],
        },
        r
    )
}

#[test]
fn test_scopes() {
    let r = DeriveResource::parse(r#"is:read bar foo in:subject in:message"#).unwrap();

    assert_eq!(
        Query {
            term: Term::And(vec![
                Term::Match(DeriveResource::Read),
                Term::Or(vec![
                    Term::Match(DeriveResource::Subject(Primary::Partial("bar"))),
                    Term::Match(DeriveResource::Message(Primary::Partial("bar"))),
                ]),
                Term::Or(vec![
                    Term::Match(DeriveResource::Subject(Primary::Partial("foo"))),
                    Term::Match(DeriveResource::Message(Primary::Partial("foo"))),
                ])
            ]),
            sorting: vec![],
        },
        r
    )
}

fn assert_query<'a>(q: &'a str, expected: Query<'a, DeriveResource<'a>>) {
    let r = DeriveResource::parse(q).unwrap();
    assert_eq!(expected, r)
}

fn assert_term<'a>(q: &'a str, term: Term<'a, DeriveResource<'a>>) {
    assert_query(
        q,
        Query {
            term,
            sorting: Default::default(),
        },
    )
}

#[test]
fn test_range() {
    assert_term(
        r"size:1..2",
        Term::Match(DeriveResource::Size(Ordered::from_range(1..2))),
    );
}

#[test]
fn test_range_1() {
    assert_term(
        r"size:*..2",
        Term::Match(DeriveResource::Size(Ordered::from_range(..2))),
    );
}

#[test]
fn test_range_2() {
    assert_term(
        r"size:1..*",
        Term::Match(DeriveResource::Size(Ordered::from_range(1..))),
    );
}

#[test]
fn test_range_3() {
    assert_term(
        r"size:*..*",
        Term::Match(DeriveResource::Size(Ordered::from_range(..))),
    );
}
