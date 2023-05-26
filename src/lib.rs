pub mod hir;
pub mod lir;
pub mod mir;
pub mod parser;

pub mod prelude {
    pub use crate::lir::*;
    pub use crate::parser::parser;
}
