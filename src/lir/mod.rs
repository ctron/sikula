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
    S: Search<'a>,
{
    Match(S::Parsed),
    Not(Box<Term<'a, S>>),
    Or(Vec<Term<'a, S>>),
    And(Vec<Term<'a, S>>),
}

impl<'a, S> Term<'a, S>
where
    S: Search<'a>,
{
    /// convenience method creating a [`Term::Not`].
    ///
    /// In case a `not` is provided to this method, its value is used instead without wrapping
    /// another "not".
    pub fn not(term: Term<'a, S>) -> Self {
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
    S: Search<'a>,
{
    pub term: Term<'a, S>,
    pub sorting: Vec<Sort<S::Sortable>>,
}

impl<'a, S> Debug for Query<'a, S>
where
    S: Search<'a> + Debug,
    S::Parsed: Debug,
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
    S: Search<'a> + PartialEq,
    S::Parsed: PartialEq,
    S::Sortable: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.sorting == other.sorting && self.term == other.term
    }
}

pub trait Search<'a>: Sized {
    type Parsed;
    type Sortable: FromQualifier;
    type Scope: FromQualifier + Eq + Hash + Clone;

    fn default_scopes() -> Vec<Self::Scope>;

    fn parse(q: &'a str) -> Result<Query<Self>, Error> {
        let query = parse_query(q)?;

        Ok(Query {
            term: Self::translate_term(query.term)?,
            sorting: translate_sorting::<Self>(query.sorting)?,
        })
    }

    fn translate_match(
        context: &Context<'a, '_, Self>,
        qualifier: Qualifier<'a>,
        expression: mir::Expression<'a>,
    ) -> Result<Term<'a, Self>, Error<'a>>;

    fn translate_term(term: mir::Term<'a>) -> Result<Term<'a, Self>, Error<'a>> {
        fn translate<'a, S: Search<'a>>(
            context: &Context<'a, '_, S>,
            term: mir::Term<'a>,
        ) -> Result<Term<'a, S>, Error<'a>> {
            match term {
                mir::Term::Not(term) => Ok(Term::not(translate(context, *term)?)),
                mir::Term::And { terms, scopes } => {
                    let context = context.push(translate_scopes::<S>(scopes)?);
                    Ok(Term::And(
                        terms
                            .into_iter()
                            .map(|term| translate(&context, term))
                            .collect::<Result<_, _>>()?,
                    ))
                }
                mir::Term::Or { terms, scopes } => {
                    let context = context.push(translate_scopes::<S>(scopes)?);
                    Ok(Term::Or(
                        terms
                            .into_iter()
                            .map(|term| translate(&context, term))
                            .collect::<Result<_, _>>()?,
                    ))
                }
                mir::Term::Match {
                    qualifier,
                    expression,
                } => Ok(S::translate_match(&context, qualifier, expression)?),
            }
        }

        translate(&Context::root(), term)
    }
}

pub struct Context<'a, 'p, S: Search<'a>> {
    parent: Option<&'p Self>,
    aggregated_scopes: Vec<S::Scope>,

    pub scopes: Vec<S::Scope>,
}

impl<'a, 'p, S: Search<'a>> Context<'a, 'p, S> {
    pub fn root() -> Self {
        Self {
            parent: None,
            scopes: S::default_scopes(),
            aggregated_scopes: S::default_scopes(),
        }
    }

    pub fn push(&'p self, scopes: Vec<S::Scope>) -> Context<'a, 'p, S> {
        let mut aggregated_scopes = self.aggregated_scopes.clone();

        for scope in &scopes {
            // FIXME: this can become expensive, we might want to find another way
            if !aggregated_scopes.contains(scope) {
                aggregated_scopes.push(scope.clone());
            }
        }

        Self {
            parent: Some(self),
            scopes,
            aggregated_scopes,
        }
    }

    pub fn aggregated_scopes(&self) -> &[S::Scope] {
        &self.aggregated_scopes
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
pub fn parse_query<'a>(q: &'a str) -> Result<mir::Query<'a>, Error> {
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
pub fn translate_sorting<'a, S: Search<'a>>(
    sorting: Vec<mir::Sort<'a>>,
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
pub fn translate_scopes<'a, S: Search<'a>>(
    scopes: Vec<Qualifier<'a>>,
) -> Result<Vec<S::Scope>, Error<'a>> {
    scopes
        .into_iter()
        .map(|qualifier| {
            S::Scope::from_qualifier(&qualifier)
                .map_err(|_| Error::UnknownScopeQualifier(qualifier))
        })
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::mir::Expression;

    struct Mock;
    impl<'a> Search<'a> for Mock {
        type Parsed = ();
        type Sortable = Vec<String>;
        type Scope = Vec<String>;

        fn default_scopes() -> Vec<Self::Scope> {
            vec![vec!["default".to_string()]]
        }

        fn translate_match(
            _context: &Context<'a, '_, Self>,
            _qualifier: Qualifier<'a>,
            _expression: Expression<'a>,
        ) -> Result<Term<'a, Self>, Error<'a>> {
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
    fn aggregated_1() {
        let ctx = Context::<Mock>::root();
        let child1 = ctx.push(vec![]);
        let child2 = child1.push(vec![vec!["child2".to_string()]]);

        let scopes = child2.aggregated_scopes();
        assert_eq!(
            scopes,
            &[vec!["default".to_string()], vec!["child2".to_string()]]
        );
    }
}
