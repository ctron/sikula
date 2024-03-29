#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term<'a> {
    Not(Box<Term<'a>>),
    And(Vec<Term<'a>>),
    Or(Vec<Term<'a>>),
    Match { invert: bool, tokens: Vec<&'a str> },
}

impl<'a> Term<'a> {
    pub fn is_empty(&self) -> bool {
        match self {
            Self::And(terms) | Self::Or(terms) => terms.is_empty(),
            _ => false,
        }
    }

    pub fn children(&self) -> Vec<&Term<'a>> {
        match self {
            Term::And(terms) => terms.iter().collect::<Vec<_>>(),
            Term::Or(terms) => terms.iter().collect::<Vec<_>>(),
            Term::Not(term) => vec![term],
            Term::Match { .. } => vec![],
        }
    }

    pub fn r#match(tokens: impl IntoIterator<Item = &'a str>) -> Self {
        Term::Match {
            tokens: tokens.into_iter().collect(),
            invert: false,
        }
    }

    pub fn dont_match(tokens: impl IntoIterator<Item = &'a str>) -> Self {
        Term::Match {
            tokens: tokens.into_iter().collect(),
            invert: true,
        }
    }

    fn or_single<F>(terms: impl IntoIterator<Item = Term<'a>>, f: F) -> Self
    where
        F: FnOnce(Vec<Self>) -> Self,
    {
        let mut terms = terms.into_iter().collect::<Vec<_>>();
        if terms.len() == 1 {
            terms.remove(0)
        } else {
            f(terms)
        }
    }

    pub fn and(terms: impl IntoIterator<Item = Term<'a>>) -> Self {
        Self::or_single(terms, Term::And)
    }

    pub fn or(terms: impl IntoIterator<Item = Term<'a>>) -> Self {
        Self::or_single(terms, Term::Or)
    }

    pub fn binary<F>(f: F, lhs: Term<'a>, rhs: Term<'a>) -> Self
    where
        F: FnOnce(Vec<Self>) -> Self,
    {
        if lhs.is_empty() {
            rhs
        } else if rhs.is_empty() {
            lhs
        } else {
            let (lhs, rhs) = (lhs, rhs);
            f(vec![lhs, rhs])
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Query<'a> {
    pub term: Term<'a>,
}
