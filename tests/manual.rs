use sikula::{mir, prelude::*};

/// same as [`ExampleResource`], but manually implemented
#[derive(Clone, Debug, PartialEq, Eq)]
enum ManualResource<'a> {
    /// Standard qualifier: `author:someone`
    Author(&'a str),
    /// Default primary: `warranty`
    Subject(Primary<'a>),
    /// Non-default primary: `warranty in:message`, to search in both: `warranty in:message in:subject`
    Message(Primary<'a>),

    /// Predicate: `is:read`
    Read,

    /// Numeric qualifier example:
    /// * `size:100` (equals)
    /// * `size:>=100` (size greater than or equals 100)
    /// * `size:100..200` (size between 100 inclusive and 200 exclusive)
    /// * `size:*..200` (size up to 200 exclusive)
    Size(Ordered<usize>),

    Sent(Ordered<time::OffsetDateTime>),

    Label(Qualified<'a, &'a str>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ManualResourceSortable {
    Author,
    Size,
    Sent,
}

impl FromQualifier for ManualResourceSortable {
    type Err = ();

    fn from_qualifier(qualifier: &Qualifier) -> Result<Self, Self::Err> {
        Ok(match qualifier.as_slice() {
            ["author"] => Self::Author,
            ["size"] => Self::Size,
            ["sent"] => Self::Sent,
            _ => return Err(()),
        })
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ManualResourceScope {
    Subject,
    Message,
}

impl FromQualifier for ManualResourceScope {
    type Err = ();

    fn from_qualifier(qualifier: &Qualifier) -> Result<Self, Self::Err> {
        Ok(match qualifier.as_slice() {
            ["subject"] => Self::Subject,
            ["message"] => Self::Message,
            _ => return Err(()),
        })
    }
}

impl<'r> Search for ManualResource<'r> {
    type Parsed<'a> = ManualResource<'a>;
    type Sortable = ManualResourceSortable;
    type Scope = ManualResourceScope;

    fn default_scopes() -> Vec<Self::Scope> {
        vec![Self::Scope::Subject]
    }

    fn translate_match<'a>(
        context: &Context<'_, Self::Parsed<'a>>,
        qualifier: Qualifier<'a>,
        expression: mir::Expression<'a>,
    ) -> Result<Term<'a, Self::Parsed<'a>>, Error<'a>> {
        let term = match expression {
            mir::Expression::Predicate => match qualifier.as_slice() {
                ["read"] => Term::Match(Self::Parsed::<'a>::Read),
                _ => return Err(Error::UnknownPredicate(qualifier)),
            },
            mir::Expression::Simple(expression) => match qualifier.as_slice() {
                [] => {
                    // primary
                    let mut terms = vec![];
                    for scope in &context.scopes {
                        let expression = match scope {
                            ManualResourceScope::Subject => Term::Match(
                                Self::Parsed::<'a>::Subject(expression.into_expression(
                                    QualifierContext::Primary,
                                    Qualifier::empty(),
                                )?),
                            ),
                            ManualResourceScope::Message => Term::Match(
                                Self::Parsed::<'a>::Message(expression.into_expression(
                                    QualifierContext::Primary,
                                    Qualifier::empty(),
                                )?),
                            ),
                        };
                        terms.push(expression);
                    }
                    Term::Or(terms)
                }
                ["message", n @ ..] => Term::Match(Self::Parsed::<'a>::Message(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                ["subject", n @ ..] => Term::Match(Self::Parsed::<'a>::Subject(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                ["author", n @ ..] => Term::Match(Self::Parsed::<'a>::Author(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                ["sent", n @ ..] => Term::Match(Self::Parsed::<'a>::Sent(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                ["size", n @ ..] => Term::Match(Self::Parsed::<'a>::Size(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                ["label", n @ ..] => Term::Match(Self::Parsed::<'a>::Label(
                    expression.into_expression(QualifierContext::Qualifier, n.into())?,
                )),
                _ => return Err(Error::UnknownQualifier(qualifier)),
            },
        };

        Ok(term)
    }
}

#[test]
fn test() {
    let r = ManualResource::parse(r#"is:read message:bar -subject:foo"#).unwrap();

    assert_eq!(
        Query {
            term: Term::And(vec![
                Term::Match(ManualResource::Read),
                Term::Match(ManualResource::Message(Primary::Equal("bar"))),
                Term::Not(Box::new(Term::Match(ManualResource::Subject(
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
    let r = ManualResource::parse(r#"is:read bar foo in:subject in:message"#).unwrap();

    assert_eq!(
        Query {
            term: Term::And(vec![
                Term::Match(ManualResource::Read),
                Term::Or(vec![
                    Term::Match(ManualResource::Subject(Primary::Partial("bar"))),
                    Term::Match(ManualResource::Message(Primary::Partial("bar"))),
                ]),
                Term::Or(vec![
                    Term::Match(ManualResource::Subject(Primary::Partial("foo"))),
                    Term::Match(ManualResource::Message(Primary::Partial("foo"))),
                ])
            ]),
            sorting: vec![],
        },
        r
    )
}

fn assert_query<'a>(q: &'a str, expected: Query<'a, ManualResource<'a>>) {
    let r = ManualResource::parse(q).unwrap();
    assert_eq!(expected, r)
}

fn assert_term<'a>(q: &'a str, term: Term<'a, ManualResource<'a>>) {
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
        Term::Match(ManualResource::Size(Ordered::from_range(1..2))),
    );
}

#[test]
fn test_range_1() {
    assert_term(
        r"size:*..2",
        Term::Match(ManualResource::Size(Ordered::from_range(..2))),
    );
}

#[test]
fn test_range_2() {
    assert_term(
        r"size:1..*",
        Term::Match(ManualResource::Size(Ordered::from_range(1..))),
    );
}

#[test]
fn test_range_3() {
    assert_term(
        r"size:*..*",
        Term::Match(ManualResource::Size(Ordered::from_range(..))),
    );
}
