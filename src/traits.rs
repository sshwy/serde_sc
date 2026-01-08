use crate::expr::{PrimitiveType, TypeExpr};

use std::collections::{BTreeMap, HashMap};

pub trait SerdeSchema {
    fn type_expr() -> TypeExpr;
}

impl SerdeSchema for () {
    fn type_expr() -> TypeExpr {
        TypeExpr::Unit
    }
}

macro_rules! impl_serde_schema_for_tuple {
    ($($T:ident),+ $(,)?) => {
        impl<$($T),+> SerdeSchema for ($($T,)+)
        where
            $($T: SerdeSchema),+
        {
            fn type_expr() -> TypeExpr {
                TypeExpr::Tuple {
                    elements: vec![$(<$T as SerdeSchema>::type_expr()),+],
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

impl<T> SerdeSchema for Option<T>
where
    T: SerdeSchema,
{
    fn type_expr() -> TypeExpr {
        TypeExpr::option(T::type_expr())
    }
}

impl<T> SerdeSchema for Vec<T>
where
    T: SerdeSchema,
{
    fn type_expr() -> TypeExpr {
        let inner = T::type_expr();
        if matches!(inner, TypeExpr::Primitive(PrimitiveType::U8)) {
            TypeExpr::Bytes
        } else {
            TypeExpr::seq(inner)
        }
    }
}

impl<K, V> SerdeSchema for HashMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn type_expr() -> TypeExpr {
        TypeExpr::map(K::type_expr(), V::type_expr())
    }
}

impl<K, V> SerdeSchema for BTreeMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn type_expr() -> TypeExpr {
        TypeExpr::map(K::type_expr(), V::type_expr())
    }
}
