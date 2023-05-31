use super::*;

use ::time::{
    format_description::well_known, macros::format_description, Date, OffsetDateTime, Time,
    UtcOffset,
};

impl<'a> FromExpression<'a> for OffsetDateTime {
    fn from_expression(
        _: QualifierContext,
        qualifier: Qualifier<'a>,
        expression: &'a str,
    ) -> Result<Self, Error<'a>> {
        if qualifier.is_empty() {
            // First try RFC33339
            if let Ok(date) = OffsetDateTime::parse(expression, &well_known::Rfc3339) {
                return Ok(date);
            }

            // A non-standard yyyy-mm-dd format
            if let Ok(date) = Date::parse(expression, &format_description!("[year]-[month]-[day]"))
            {
                return Ok(date.with_time(Time::MIDNIGHT).assume_offset(UtcOffset::UTC));
            }

            Err(Error::Parser("Error parsing date".to_string()))
        } else {
            Err(Error::UnknownQualifier(qualifier))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let time = OffsetDateTime::from_expression(
            QualifierContext::Qualifier,
            Qualifier::empty(),
            "2022-01-01",
        );
        assert!(time.is_ok());

        let time = OffsetDateTime::from_expression(
            QualifierContext::Qualifier,
            Qualifier::empty(),
            "2023-03-23T11:14:00Z",
        );
        assert!(time.is_ok());
    }
}
