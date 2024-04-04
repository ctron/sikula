use crate::prelude::*;
use sea_orm::{sea_query::IntoCondition, ColumnTrait, Condition, Value};
use std::ops::Bound;

/// Translate a term into a condition.
///
/// This function takes care of all the basic transformation and leaves the actual mapping to the
/// provided translate function.
pub fn translate_term<'a, S, F, FO>(term: Term<'a, S>, translate: &F) -> Condition
where
    S: Search,
    F: Fn(S::Parsed<'a>) -> FO,
    FO: IntoCondition,
{
    match term {
        Term::Not(term) => translate_term(*term, translate).not(),
        Term::Or(terms) => {
            let mut result = Condition::any();
            for term in terms {
                result = result.add(translate_term(term, translate));
            }
            result
        }
        Term::And(terms) => {
            let mut result = Condition::all();
            for term in terms {
                result = result.add(translate_term(term, translate));
            }
            result
        }
        Term::Match(m) => translate(m).into_condition(),
    }
}

/// translate an `Ordered` value into a SeaORM condition.
pub fn translate_ordered<C, T>(column: C, value: Ordered<T>) -> Condition
where
    C: ColumnTrait,
    T: Ord + Into<Value>,
{
    match value {
        Ordered::Equal(value) => column.eq(value).into_condition(),
        Ordered::Less(value) => column.lt(value).into_condition(),
        Ordered::LessEqual(value) => column.lte(value).into_condition(),
        Ordered::Greater(value) => column.gt(value).into_condition(),
        Ordered::GreaterEqual(value) => column.gte(value).into_condition(),
        Ordered::Range(min, max) => {
            let mut result = Condition::all();
            if let Some(min) =
                translate_bound(min, |value| column.gte(value), |value| column.gt(value))
            {
                result = result.add(min);
            }
            if let Some(max) =
                translate_bound(max, |value| column.lte(value), |value| column.lt(value))
            {
                result = result.add(max);
            }
            result
        }
    }
}

/// Default translation of a value bound into a SeaORM condition.
///
/// This will call either the result or included, excluded or `None`, depending on the input value.
pub fn translate_bound<T, FI, FIO, FE, FEO>(
    bound: Bound<T>,
    included: FI,
    excluded: FE,
) -> Option<Condition>
where
    T: Into<Value>,
    FI: FnOnce(Value) -> FIO,
    FIO: IntoCondition,
    FE: FnOnce(Value) -> FEO,
    FEO: IntoCondition,
{
    match bound {
        Bound::Unbounded => None,
        Bound::Included(value) => Some(included(value.into()).into_condition()),
        Bound::Excluded(value) => Some(excluded(value.into()).into_condition()),
    }
}
