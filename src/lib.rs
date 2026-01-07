use crate::expr::TypeExpr;

pub mod expr;

pub use serde_schema_macros::SerdeSchema;

pub trait SerdeSchema {
    fn serde_schema() -> TypeExpr;
}
