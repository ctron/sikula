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

pub mod hir;
pub mod lir;
pub mod mir;
pub mod parser;

/// The prelude
pub mod prelude {
    pub use crate::lir::*;
    pub use crate::mir::Qualifier;
    pub use crate::parser::parser;
    pub use sikula_macros::Search;
}

pub use sikula_macros::Search;

pub use ::chumsky;
