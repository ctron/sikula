#[cfg(any(feature = "time", test))]
mod time;

#[cfg(any(feature = "time", test))]
pub use self::time::*;

use crate::mir::{self, Direction, Qualifier};
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
            Ordered::LessEqual(T::from_expression(context, qualifier, expression)?)
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sort<S> {
    pub qualifier: S,
    pub direction: Direction,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<'a, R>
where
    R: Resource<'a>,
{
    Match(R::Parsed),
    Not(Box<Term<'a, R>>),
    Or(Vec<Term<'a, R>>),
    And(Vec<Term<'a, R>>),
}

impl<'a, R> Term<'a, R>
where
    R: Resource<'a>,
{
    pub fn compact(self) -> Self {
        match self {
            Self::Or(mut terms) if terms.len() == 1 => terms.pop().unwrap(),
            Self::And(mut terms) if terms.len() == 1 => terms.pop().unwrap(),
            _ => self,
        }
    }
}

pub struct Query<'a, R>
where
    R: Resource<'a>,
{
    pub term: Term<'a, R>,
    pub sorting: Vec<Sort<R::Sortable>>,
}

impl<'a, R> Debug for Query<'a, R>
where
    R: Resource<'a> + Debug,
    R::Parsed: Debug,
    R::Sortable: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Query")
            .field("terms", &self.term)
            .field("sorting", &self.sorting)
            .finish()
    }
}

impl<'a, R> PartialEq for Query<'a, R>
where
    R: Resource<'a> + PartialEq,
    R::Parsed: PartialEq,
    R::Sortable: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.sorting == other.sorting && self.term == other.term
    }
}

pub trait Resource<'a>: Sized {
    type Parsed;
    type Sortable: FromQualifier;
    type Scope: FromQualifier + Eq + Hash;

    fn default_scopes() -> Vec<Self::Scope>;

    fn parse_query(q: &'a str) -> Result<Query<Self>, Error>;
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
