#![doc = include_str!("../../README.md")]

pub mod context;
pub mod expr;
pub mod registry;
pub mod traits;

pub use serde_sc_macros::SerdeSchema;
pub use traits::SerdeSchema;

// This macro constructs a Remote variant of TypeExpr with the given type's path and TypeId.
// It is useful for representing types not defined in the local crate.
#[macro_export]
macro_rules! remote_type {
    ($ty:ty) => {
        $crate::expr::TypeExpr::Remote {
            path: ::std::borrow::Cow::Borrowed(stringify!($ty)),
            type_id: ::std::any::TypeId::of::<$ty>(),
        }
    };
}
