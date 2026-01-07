use crate::expr::{PrimitiveType, TypeExpr};

use std::collections::{BTreeMap, HashMap};

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

impl<T> SerdeSchema for Option<T>
where
    T: SerdeSchema,
{
    fn serde_schema() -> TypeExpr {
        TypeExpr::option(T::serde_schema())
    }
}

impl<T> SerdeSchema for Vec<T>
where
    T: SerdeSchema,
{
    fn serde_schema() -> TypeExpr {
        let inner = T::serde_schema();
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
    fn serde_schema() -> TypeExpr {
        TypeExpr::map(K::serde_schema(), V::serde_schema())
    }
}

impl<K, V> SerdeSchema for BTreeMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn serde_schema() -> TypeExpr {
        TypeExpr::map(K::serde_schema(), V::serde_schema())
    }
}
