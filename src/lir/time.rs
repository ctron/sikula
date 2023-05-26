use super::*;

use ::time::{format_description, OffsetDateTime};

impl<'a> FromExpression<'a> for OffsetDateTime {
    fn from_expression(
        _: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if qualifier.is_empty() {
            OffsetDateTime::parse(expression, &format_description::well_known::Rfc3339)
                .map_err(|err| Error::Expression(err.to_string()))
        } else {
            Err(Error::UnknownQualifier(qualifier))
        }
    }
}
