use crate::expr::{PrimitiveType, TypeExpr};

use std::{
    any::TypeId,
    collections::{BTreeMap, HashMap, HashSet},
};

/// Context used during schema building, such as for tracking which types are being processed to prevent infinite recursion.
#[derive(Debug, Default)]
pub struct Context {
    pending: HashSet<TypeId>,
}

impl Context {
    pub fn is_pending<T>(&self) -> bool
    where
        T: 'static,
    {
        self.pending.contains(&TypeId::of::<T>())
    }
    pub fn set_pending<T>(&mut self, pending: bool)
    where
        T: 'static,
    {
        let id = TypeId::of::<T>();
        if pending {
            self.pending.insert(id);
        } else {
            self.pending.remove(&id);
        }
    }
}

/// Trait for describing how a type maps to a TypeExpr schema for serialization/deserialization.
pub trait SerdeSchema {
    /// Builds the schema type expression for the implementing type, possibly using and updating the context.
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr;

    /// Returns the type schema expression using a fresh default context.
    fn type_expr() -> TypeExpr {
        Self::build_type_expr(&mut Default::default())
    }
}

impl SerdeSchema for () {
    fn build_type_expr(_ctxt: &mut Context) -> TypeExpr {
        TypeExpr::Unit
    }
}

macro_rules! impl_serde_schema_for_tuple {
    ($($T:ident),+ $(,)?) => {
        impl<$($T),+> SerdeSchema for ($($T,)+)
        where
            $($T: SerdeSchema),+
        {
            fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
                TypeExpr::Tuple {
                    elements: vec![$(<$T as SerdeSchema>::build_type_expr(ctxt)),+],
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
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        TypeExpr::option(T::build_type_expr(ctxt))
    }
}

impl<T> SerdeSchema for Vec<T>
where
    T: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        let inner = T::build_type_expr(ctxt);
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
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        TypeExpr::map(K::build_type_expr(ctxt), V::build_type_expr(ctxt))
    }
}

impl<K, V> SerdeSchema for BTreeMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        TypeExpr::map(K::build_type_expr(ctxt), V::build_type_expr(ctxt))
    }
}
