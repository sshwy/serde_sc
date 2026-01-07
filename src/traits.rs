use crate::expr::TypeExpr;

pub trait SerdeSchema {
    fn serde_schema() -> TypeExpr;
}
