//! Simple Query Language
//!
//! This is a parser which allows implementing a strongly typed, simple query syntax for custom
//! resources.
//!
//! ## Example
//!
//! Define a resource that you want to search in:
//!
//! ```rust
//! use sikula::prelude::*;
//!
//! #[derive(Search)]
//! pub enum MyResource<'a> {
//!     #[search(scope, default)]
//!     Subject(Primary<'a>),
//!     #[search(scope)]
//!     Body(Primary<'a>),
//!
//!     Sender(&'a str),
//!
//!     #[search(sort)]
//!     Sent(Ordered<time::OffsetDateTime>),
//!     #[search(sort)]
//!     Size(Ordered<usize>),
//!
//!     #[search(sort)]
//!     Header(Qualified<'a, &'a str>),
//!
//!     Read,
//!     Important,
//! }
//! ```
//! Next, having your query, parse it into the enum:
//!
//! ```rust
//! use sikula::prelude::*;
//!
//! # #[derive(Search)]
//! # pub enum MyResource<'a> {
//! #    #[search(scope, default)]
//! #    Subject(Primary<'a>),
//! #    #[search(scope)]
//! #    Body(Primary<'a>),
//! #
//! #    Sender(&'a str),
//! #
//! #    #[search(sort)]
//! #    Sent(Ordered<time::OffsetDateTime>),
//! #    #[search(sort)]
//! #    Size(Ordered<usize>),
//! #
//! #    #[search(sort)]
//! #    Header(Qualified<'a, &'a str>),
//! #
//! #    Read,
//! #    Important,
//! # }
//!
//! # pub type MyItem = ();
//! fn search(query: &str) -> Result<Vec<MyItem>, Error> {
//!   let query = MyResource::parse(query)?;
//!   Ok(perform_search(query))
//! }
//!
//! fn perform_search<'a>(query: Query<'a, MyResource<'a>>) -> Vec<MyItem> {
//!   // Here you need to translate the query structure into some query for the target store
//!   // (like SQL), or manually apply a filter in-memory to items. Or do a mix, whatever makes
//!   // sense for your data.
//!   todo!("Implement search")
//! }
//! ```
//!
//! Now, you can do the following queries:
//!
//! | Query                                             | Retrieves all entries…                                                                            |
//! |---------------------------------------------------|---------------------------------------------------------------------------------------------------|
//! | `foo`                                             | … containing "foo" in the "subject"                                                               |
//! | `foo in:subject in:message`                       | … containing "foo" in either "subject" or "body"                                                  |
//! | `foo in:subject in:message is:read`               | … containing "foo" in either "subject" or "body" being "read"                                     |
//! | `foo bar`                                         | … containing "foo" and "bar" in the subject                                                       |
//! | `size:>10000`                                     | … having a size greater than 10000                                                                |
//! | `size:100..200`                                   | … having a size between 100 (inclusive) and 200 (exclusive)                                       |
//!
//! See more examples in README.

/// High-level intermediate representation
#[doc(hidden)]
pub mod hir;
/// Low-level intermediate representation
pub mod lir;
#[doc(hidden)]
/// Mid-level intermediate representation
pub mod mir;
/// Basic parsing
pub mod parser;

/// The prelude
pub mod prelude {
    pub use crate::lir::*;
    pub use crate::mir::Qualifier;
    pub use crate::parser::parser;
    pub use sikula_macros::Search;
}

/// A derive implementing the [`crate::prelude::Search`] trait.
pub use sikula_macros::Search;

/// The chumsky version used by this crate.
pub use ::chumsky;
