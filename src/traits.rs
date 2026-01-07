use crate::expr::{PrimitiveType, TypeExpr};

use std::collections::{BTreeMap, HashMap};

pub trait SerdeSchema {
    fn serde_sc() -> TypeExpr;
}

impl SerdeSchema for () {
    fn serde_sc() -> TypeExpr {
        TypeExpr::Unit
    }
}

macro_rules! impl_serde_schema_for_tuple {
    ($($T:ident),+ $(,)?) => {
        impl<$($T),+> SerdeSchema for ($($T,)+)
        where
            $($T: SerdeSchema),+
        {
            fn serde_sc() -> TypeExpr {
                TypeExpr::Tuple {
                    elements: vec![$(<$T as SerdeSchema>::serde_sc()),+],
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
    fn serde_sc() -> TypeExpr {
        TypeExpr::option(T::serde_sc())
    }
}

impl<T> SerdeSchema for Vec<T>
where
    T: SerdeSchema,
{
    fn serde_sc() -> TypeExpr {
        let inner = T::serde_sc();
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
    fn serde_sc() -> TypeExpr {
        TypeExpr::map(K::serde_sc(), V::serde_sc())
    }
}

impl<K, V> SerdeSchema for BTreeMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn serde_sc() -> TypeExpr {
        TypeExpr::map(K::serde_sc(), V::serde_sc())
    }
}
