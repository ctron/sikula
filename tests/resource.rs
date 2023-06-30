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
