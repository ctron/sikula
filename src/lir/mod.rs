#[cfg(any(feature = "time", test))]
mod time;

#[cfg(any(feature = "time", test))]
pub use self::time::*;

use crate::{
    chumsky::Parser,
    mir::{self, Direction, Qualifier},
    parser::parser,
};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::{Bound, RangeBounds};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ordered<T: Ord> {
    Equal(T),
    Greater(T),
    GreaterEqual(T),
    Less(T),
    LessEqual(T),
    Range(Bound<T>, Bound<T>),
}

impl<T> Ordered<T>
where
    T: Ord + Clone,
{
    pub fn from_range<R>(range: R) -> Self
    where
        R: RangeBounds<T>,
    {
        Ordered::Range(range.start_bound().cloned(), range.end_bound().cloned())
    }
}

impl<'a, T> FromExpression<'a> for Ordered<T>
where
    T: Ord + FromExpression<'a>,
{
    fn from_expression(
        context: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if !qualifier.is_empty() {
            return Err(Error::UnknownQualifier(qualifier));
        }

        Ok(if let Some(expression) = expression.strip_prefix(">=") {
            Ordered::GreaterEqual(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix('>') {
            Ordered::Greater(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix("<=") {
            Ordered::LessEqual(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix('<') {
            Ordered::Less(T::from_expression(context, qualifier, expression)?)
        } else {
            match expression.split_once("..") {
                Some(m) => {
                    let (from, to) = match m {
                        ("*", "*") => (Bound::Unbounded, Bound::Unbounded),
                        ("*", to) => (
                            Bound::Unbounded,
                            Bound::Excluded(T::from_expression(context, qualifier, to)?),
                        ),
                        (from, "*") => (
                            Bound::Included(T::from_expression(context, qualifier.clone(), from)?),
                            Bound::Unbounded,
                        ),
                        (from, to) => (
                            Bound::Included(T::from_expression(context, qualifier.clone(), from)?),
                            Bound::Excluded(T::from_expression(context, qualifier, to)?),
                        ),
                    };

                    Ordered::Range(from, to)
                }
                None => Ordered::Equal(T::from_expression(context, qualifier, expression)?),
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PartialOrdered<T: PartialOrd> {
    Greater(T),
    GreaterEqual(T),
    Less(T),
    LessEqual(T),
    Range(Bound<T>, Bound<T>),
}

impl<T> PartialOrdered<T>
where
    T: PartialOrd + Clone,
{
    pub fn from_range<R>(range: R) -> Self
    where
        R: RangeBounds<T>,
    {
        PartialOrdered::Range(range.start_bound().cloned(), range.end_bound().cloned())
    }
}

impl<'a, T> FromExpression<'a> for PartialOrdered<T>
where
    T: PartialOrd + FromExpression<'a>,
{
    fn from_expression(
        context: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if !qualifier.is_empty() {
            return Err(Error::UnknownQualifier(qualifier));
        }

        Ok(if let Some(expression) = expression.strip_prefix(">=") {
            PartialOrdered::GreaterEqual(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix('>') {
            PartialOrdered::Greater(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix("<=") {
            PartialOrdered::LessEqual(T::from_expression(context, qualifier, expression)?)
        } else if let Some(expression) = expression.strip_prefix('<') {
            PartialOrdered::Less(T::from_expression(context, qualifier, expression)?)
        } else {
            match expression.split_once("..") {
                Some(m) => {
                    let (from, to) = match m {
                        ("*", "*") => (Bound::Unbounded, Bound::Unbounded),
                        ("*", to) => (
                            Bound::Unbounded,
                            Bound::Excluded(T::from_expression(context, qualifier, to)?),
                        ),
                        (from, "*") => (
                            Bound::Included(T::from_expression(context, qualifier.clone(), from)?),
                            Bound::Unbounded,
                        ),
                        (from, to) => (
                            Bound::Included(T::from_expression(context, qualifier.clone(), from)?),
                            Bound::Excluded(T::from_expression(context, qualifier, to)?),
                        ),
                    };

                    PartialOrdered::Range(from, to)
                }
                None => PartialOrdered::Range(Bound::Unbounded, Bound::Unbounded),
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Qualified<'a, T>
where
    T: FromExpression<'a>,
{
    pub qualifier: Qualifier<'a>,
    pub expression: T,
}

impl<'a, T> FromExpression<'a> for Qualified<'a, T>
where
    T: FromExpression<'a>,
{
    fn from_expression(
        context: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        Ok(Self {
            qualifier,
            expression: expression.into_expression(context, Qualifier::empty())?,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
pub enum Error<'a> {
    #[error("Parser error: {0}")]
    Parser(String),
    #[error(transparent)]
    ParserMir(#[from] mir::Error),
    #[error("Invalid expression: {0}")]
    Expression(String),
    #[error("Unknown predicate: {0}")]
    UnknownPredicate(Qualifier<'a>),
    #[error("Unknown qualifier: {0}")]
    UnknownQualifier(Qualifier<'a>),
    #[error("Unknown sort qualifier: {0}")]
    UnknownSortQualifier(Qualifier<'a>),
    #[error("Unknown scope qualifier: {0}")]
    UnknownScopeQualifier(Qualifier<'a>),
    #[error("Unsupported primary: {0}")]
    UnsupportedPrimary(Qualifier<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sort<S> {
    pub qualifier: S,
    pub direction: Direction,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<'a, S>
where
    S: Search,
{
    Match(S::Parsed<'a>),
    Not(Box<Term<'a, S>>),
    Or(Vec<Term<'a, S>>),
    And(Vec<Term<'a, S>>),
}

impl<'a, S> Term<'a, S>
where
    S: Search,
{
    /// convenience method creating a [`Term::Not`].
    ///
    /// In case a `not` is provided to this method, its value is used instead without wrapping
    /// another "not".
    pub fn new_not(term: Term<'a, S>) -> Self {
        match term {
            Term::Not(term) => *term,
            otherwise => Term::Not(Box::new(otherwise)),
        }
    }

    pub fn compact(self) -> Self {
        match self {
            Self::Or(mut terms) if terms.len() == 1 => terms.pop().unwrap(),
            Self::And(mut terms) if terms.len() == 1 => terms.pop().unwrap(),
            _ => self,
        }
    }
}

pub struct Query<'a, S>
where
    S: Search,
{
    pub term: Term<'a, S>,
    pub sorting: Vec<Sort<S::Sortable>>,
}

impl<'a, S> Debug for Query<'a, S>
where
    S: Search + Debug,
    S::Parsed<'a>: Debug,
    S::Sortable: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Query")
            .field("terms", &self.term)
            .field("sorting", &self.sorting)
            .finish()
    }
}

impl<'a, S> PartialEq for Query<'a, S>
where
    S: Search + PartialEq,
    S::Parsed<'a>: PartialEq,
    S::Sortable: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.sorting == other.sorting && self.term == other.term
    }
}

/// A trait to define a resource as searchable.
///
/// *NOTE:* This is a trait which is normally implemented through `#[derive(Search)]`.
pub trait Search: Sized {
    type Parsed<'a>: Search;
    type Sortable: FromQualifier;
    type Scope: FromQualifier + Eq + Hash + Clone;

    fn default_scopes() -> Vec<Self::Scope>;

    fn parse<'a>(q: &'a str) -> Result<Query<Self::Parsed<'a>>, Error> {
        Self::parse_from(parse_query(q)?)
    }

    fn parse_from<'a>(query: mir::Query<'a>) -> Result<Query<Self::Parsed<'a>>, Error> {
        Ok(Query {
            term: Self::translate_term(query.term)?,
            sorting: translate_sorting::<Self::Parsed<'a>>(query.sorting)?,
        })
    }

    fn translate_match<'a>(
        context: &Context<'_, Self::Parsed<'a>>,
        qualifier: Qualifier<'a>,
        expression: mir::Expression<'a>,
    ) -> Result<Term<'a, Self::Parsed<'a>>, Error<'a>>;

    fn translate_term<'a>(term: mir::Term<'a>) -> Result<Term<Self::Parsed<'a>>, Error<'a>> {
        translate::<Self>(&Context::root(), term)
    }
}

fn translate<'a, S: Search>(
    context: &Context<'_, S::Parsed<'a>>,
    term: mir::Term<'a>,
) -> Result<Term<'a, S::Parsed<'a>>, Error<'a>> {
    match term {
        mir::Term::Not(term) => Ok(Term::new_not(translate::<S>(context, *term)?)),
        mir::Term::And { terms, scopes } => {
            let context = context.push(translate_scopes::<S>(scopes)?);
            Ok(Term::And(
                terms
                    .into_iter()
                    .map(|term| translate::<S>(&context, term))
                    .collect::<Result<_, _>>()?,
            ))
        }
        mir::Term::Or { terms, scopes } => {
            let context = context.push(translate_scopes::<S>(scopes)?);
            Ok(Term::Or(
                terms
                    .into_iter()
                    .map(|term| translate::<S>(&context, term))
                    .collect::<Result<_, _>>()?,
            ))
        }
        mir::Term::Match {
            qualifier,
            expression,
        } => Ok(S::translate_match(context, qualifier, expression)?),
    }
}

pub struct Context<'p, S: Search> {
    parent: Option<&'p Self>,

    pub scopes: Vec<S::Scope>,
}

impl<'p, S: Search> Context<'p, S> {
    pub fn root() -> Self {
        Self {
            parent: None,
            scopes: S::default_scopes(),
        }
    }

    pub fn push(&'p self, scopes: Vec<S::Scope>) -> Context<'p, S> {
        let scopes = if scopes.is_empty() {
            self.scopes.clone()
        } else {
            scopes
        };

        Self {
            parent: Some(self),
            scopes,
        }
    }

    /// call the provided function for this, walking up the parent chain, calling it for every parent
    pub fn walk_up<T, F>(&self, init: T, f: F) -> T
    where
        F: Fn(&Self, T) -> T,
    {
        let v = f(self, init);
        if let Some(parent) = &self.parent {
            parent.walk_up(v, f)
        } else {
            v
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum QualifierContext {
    Primary,
    Qualifier,
}

pub trait FromQualifier: Sized {
    type Err;

    fn from_qualifier(qualifier: &Qualifier) -> Result<Self, Self::Err>;
}

/// Convert into an array strings, mainly for testing.
impl FromQualifier for Vec<String> {
    type Err = ();

    fn from_qualifier(qualifier: &Qualifier) -> Result<Self, Self::Err> {
        Ok(qualifier.0.iter().map(|s| s.to_string()).collect())
    }
}

pub trait FromExpression<'a>: Sized {
    fn from_expression(
        context: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>>;
}

impl<'a> FromExpression<'a> for &'a str {
    fn from_expression(
        _: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if qualifier.is_empty() {
            Ok(expression)
        } else {
            Err(Error::UnknownQualifier(qualifier))
        }
    }
}

impl<'a> FromExpression<'a> for String {
    fn from_expression(
        _: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if qualifier.is_empty() {
            Ok(expression.to_string())
        } else {
            Err(Error::UnknownQualifier(qualifier))
        }
    }
}

macro_rules! from_str {
    ($n:ty) => {
        impl<'a> FromExpression<'a> for $n {
            fn from_expression(
                _: QualifierContext,
                qualifier: Qualifier<'a>,
                expression: &'a str,
            ) -> Result<Self, Error<'a>> {
                if qualifier.is_empty() {
                    expression
                        .parse::<$n>()
                        .map_err(|err| Error::Expression(err.to_string()))
                } else {
                    Err(Error::UnknownQualifier(qualifier))
                }
            }
        }
    };
}

from_str!(i32);
from_str!(i64);
from_str!(u32);
from_str!(u64);
from_str!(usize);
from_str!(isize);
from_str!(f32);
from_str!(f64);

pub trait IntoExpression<'a, T>: Sized {
    fn into_expression(
        self,
        context: QualifierContext,
        qualifier: Qualifier<'a>,
    ) -> Result<T, Error<'a>>;
}

impl<'a, T> IntoExpression<'a, T> for &'a str
where
    T: FromExpression<'a>,
{
    fn into_expression(
        self,
        context: QualifierContext,
        qualifier: Qualifier<'a>,
    ) -> Result<T, Error<'a>> {
        T::from_expression(context, qualifier, self)
    }
}

/// A primary search term.
///
/// This is intended to be used by `#[search(scope)]` or `#[search(default)]` values. If the term
/// is presented as an unqualified term, it will be translated into a [`Self::Partial`] variant, if
/// it is being used as part of a qualified term, it will be an [`Self::Equal`] variant.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Primary<'a> {
    Equal(&'a str),
    Partial(&'a str),
}

impl<'a> FromExpression<'a> for Primary<'a> {
    fn from_expression(
        context: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if !qualifier.is_empty() {
            return Err(Error::UnknownQualifier(qualifier));
        }

        Ok(match context {
            QualifierContext::Primary => Primary::Partial(expression),
            QualifierContext::Qualifier => Primary::Equal(expression),
        })
    }
}

/// Convenience method to parse a string into a [mir::Query], when being used in [`crate::lir`].
#[doc(hidden)]
pub fn parse_query(q: &str) -> Result<mir::Query, Error> {
    Ok(mir::Query::parse(
        parser().parse(q).into_result().map_err(|s| {
            Error::Parser(
                s.into_iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        })?,
    )?)
}

/// Translate from [`mir::Sort`] into [`lir::Sort`].
#[doc(hidden)]
pub fn translate_sorting<S: Search>(
    sorting: Vec<mir::Sort>,
) -> Result<Vec<Sort<S::Sortable>>, Error> {
    sorting
        .into_iter()
        .map(|sort| {
            Ok(Sort {
                qualifier: S::Sortable::from_qualifier(&sort.qualifier)
                    .map_err(|_| Error::UnknownSortQualifier(sort.qualifier))?,
                direction: sort.direction,
            })
        })
        .collect()
}

/// Translate from [`mir::Scope`] into [`lir::Scope`].
#[doc(hidden)]
pub fn translate_scopes<'a, S: Search>(
    scopes: Vec<Qualifier>,
) -> Result<Vec<<S::Parsed<'a> as Search>::Scope>, Error> {
    scopes
        .into_iter()
        .map(|qualifier| {
            <S::Parsed<'a> as Search>::Scope::from_qualifier(&qualifier)
                .map_err(|_| Error::UnknownScopeQualifier(qualifier))
        })
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::mir::Expression;

    struct Mock;
    impl Search for Mock {
        type Parsed<'a> = Mock;
        type Sortable = Vec<String>;
        type Scope = Vec<String>;

        fn default_scopes() -> Vec<Self::Scope> {
            vec![vec!["default".to_string()]]
        }

        fn translate_match<'a>(
            _context: &Context<'_, Self>,
            _qualifier: Qualifier<'a>,
            _expression: Expression<'a>,
        ) -> Result<Term<'a, Self::Parsed<'a>>, Error<'a>> {
            todo!()
        }
    }

    #[test]
    fn walk_up() {
        let ctx = Context::<Mock>::root();
        let child1 = ctx.push(vec![]);
        let child2 = child1.push(vec![]);

        let v = child2.walk_up(0, |_, v| v + 1);
        assert_eq!(v, 3);
    }

    #[test]
    fn scopes_1() {
        let ctx = Context::<Mock>::root();
        let child1 = ctx.push(vec![]);
        let child2 = child1.push(vec![vec!["child2".to_string()]]);

        // we should only get the last defined (non-empty list)
        assert_eq!(&ctx.scopes, &[vec!["default".to_string()]]);
        assert_eq!(&child1.scopes, &[vec!["default".to_string()]]);
        assert_eq!(&child2.scopes, &[vec!["child2".to_string()]]);
    }
}
