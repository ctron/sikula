include!("resource.rs");

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

#[test]
fn test_scopes_2() {
    // query "in" a scope, but not the default, the default must be gone then
    let r = DeriveResource::parse(r#"bar foo in:message"#).unwrap();

    assert_eq!(
        Query {
            term: Term::And(vec![
                Term::Match(DeriveResource::Message(Primary::Partial("bar"))),
                Term::Match(DeriveResource::Message(Primary::Partial("foo")))
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

fn assert_err(q: &str) {
    let r = DeriveResource::parse(q);
    assert!(r.is_err(), "Should fail, was: {r:?}");
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

#[test]
fn test_qualifier_1() {
    assert_term(r#"author:foo"#, Term::Match(DeriveResource::Author("foo")));
}

#[test]
fn test_qualifier_2() {
    assert_term(
        r#"author:"with spaces""#,
        Term::Match(DeriveResource::Author("with spaces")),
    );
}

#[test]
fn test_multiple_1() {
    assert_term(
        r#"author:foo author:bar author:baz"#,
        Term::And(vec![
            Term::Match(DeriveResource::Author("foo")),
            Term::Match(DeriveResource::Author("bar")),
            Term::Match(DeriveResource::Author("baz")),
        ]),
    );
}

#[test]
fn test_multiple_2() {
    assert_term(
        r#"author:foo,"with spaces",baz"#,
        Term::Or(vec![
            Term::Match(DeriveResource::Author("foo")),
            Term::Match(DeriveResource::Author("with spaces")),
            Term::Match(DeriveResource::Author("baz")),
        ]),
    );
}

#[test]
fn test_not_1() {
    assert_term(
        r#"A NOT B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Not(Box::new(Term::Match(DeriveResource::Subject(
                Primary::Partial("B"),
            )))),
        ]),
    );
}

#[test]
fn test_not_2() {
    assert_term(
        r#"A NOT NOT B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_not_3() {
    assert_term(
        r#"NOT NOT A NOT NOT B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_and_1() {
    assert_term(
        r#"A B C"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("C"))),
        ]),
    );
}

#[test]
fn test_and_2() {
    assert_term(
        r#"A AND B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_or_1() {
    assert_term(
        r#"A OR B"#,
        Term::Or(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_or_and_not_1() {
    assert_term(
        r#"A OR NOT B AND C"#,
        Term::Or(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::And(vec![
                Term::Not(Box::new(Term::Match(DeriveResource::Subject(
                    Primary::Partial("B"),
                )))),
                Term::Match(DeriveResource::Subject(Primary::Partial("C"))),
            ]),
        ]),
    );
}

#[test]
fn test_or_and_not_2() {
    assert_term(
        r#"A OR ( NOT (B AND C) )"#,
        Term::Or(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Not(Box::new(Term::And(vec![
                Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
                Term::Match(DeriveResource::Subject(Primary::Partial("C"))),
            ]))),
        ]),
    );
}

#[test]
fn test_escape_1() {
    assert_term(
        r#"A "OR" B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("OR"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_escape_2() {
    assert_term(
        r#"A "(" B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("("))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_escape_3() {
    assert_term(
        r#"A "(" B"#,
        Term::And(vec![
            Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
            Term::Match(DeriveResource::Subject(Primary::Partial("("))),
            Term::Match(DeriveResource::Subject(Primary::Partial("B"))),
        ]),
    );
}

#[test]
fn test_just_not() {
    assert_err(r#"NOT"#);
}

#[test]
fn test_multi_not() {
    assert_term(
        r#"NOT NOT A"#,
        Term::Match(DeriveResource::Subject(Primary::Partial("A"))),
    );
}

#[test]
fn test_just_and() {
    assert_err(r#"AND"#);
}

#[test]
fn test_double_ops_1() {
    assert_err(r#"A AND AND B"#);
}

#[test]
fn test_double_ops_2() {
    assert_err(r#"A OR AND B"#);
}

#[test]
fn test_brackets_1() {
    assert_err(r#"("#);
}

#[test]
fn test_brackets_2() {
    assert_term(r#"()"#, Term::And(vec![]));
}

#[test]
fn test_brackets_3() {
    assert_err(r#"((A )"#);
}
