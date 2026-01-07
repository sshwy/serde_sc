use crate::expr::TypeExpr;

pub trait SerdeSchema {
    fn serde_schema() -> TypeExpr;
}

impl SerdeSchema for () {
    fn serde_schema() -> TypeExpr {
        TypeExpr::Unit
    }
}

macro_rules! impl_serde_schema_for_tuple {
    ($($T:ident),+ $(,)?) => {
        impl<$($T),+> SerdeSchema for ($($T,)+)
        where
            $($T: SerdeSchema),+
        {
            fn serde_schema() -> TypeExpr {
                TypeExpr::Tuple {
                    elements: vec![$(<$T as SerdeSchema>::serde_schema()),+],
                }
            }
        }
    };
}

impl_serde_schema_for_tuple!(T0);
impl_serde_schema_for_tuple!(T0, T1);
impl_serde_schema_for_tuple!(T0, T1, T2);
impl_serde_schema_for_tuple!(T0, T1, T2, T3);
impl_serde_schema_for_tuple!(T0, T1, T2, T3, T4);
impl_serde_schema_for_tuple!(T0, T1, T2, T3, T4, T5);
impl_serde_schema_for_tuple!(T0, T1, T2, T3, T4, T5, T6);
