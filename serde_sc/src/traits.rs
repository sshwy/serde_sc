use crate::{
    context::Context,
    expr::{PrimitiveType, TypeExpr},
    registry::RegistryContext,
};

use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
    sync::Arc,
};

/// Trait for describing how a type maps to a TypeExpr schema for serialization/deserialization.
pub trait SerdeSchema: 'static {
    /// Builds the schema type expression for the implementing type, possibly using and updating the context.
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr;

    /// Builds the schema type expression for the implementing type,
    /// tracking if the type is already being processed to detect recursion.
    fn build_type_expr_no_recursion(ctxt: &mut Context) -> TypeExpr
    where
        Self: Sized,
    {
        if ctxt.is_pending::<Self>() {
            panic!(
                "Recursive type detected: {}, pending: {:?}",
                std::any::type_name::<Self>(),
                ctxt.pending()
            );
        }
        ctxt.set_pending::<Self>(true);
        let expr = Self::build_type_expr(ctxt);
        ctxt.set_pending::<Self>(false);
        expr
    }

    /// Registers the type schema expression of the implementing type with the given registry.
    ///
    /// The default implementation only inserts the type schema expression of the implementing type itself.
    /// Override this method to insert dependent types.
    fn on_register(registry: &mut RegistryContext)
    where
        Self: Sized,
    {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }

    /// Returns the type schema expression using a fresh default context.
    fn type_expr() -> TypeExpr
    where
        Self: Sized,
    {
        Self::build_type_expr_no_recursion(&mut Default::default())
    }
}

impl SerdeSchema for () {
    fn build_type_expr(_ctxt: &mut Context) -> TypeExpr {
        TypeExpr::Unit
    }
}

macro_rules! impl_serde_schema_for_primitive {
    ($ty:ty, $prim:ident) => {
        impl SerdeSchema for $ty {
            fn build_type_expr(_ctxt: &mut Context) -> TypeExpr {
                TypeExpr::Primitive(PrimitiveType::$prim)
            }
        }
    };
}

impl_serde_schema_for_primitive!(bool, Bool);
impl_serde_schema_for_primitive!(i8, I8);
impl_serde_schema_for_primitive!(i16, I16);
impl_serde_schema_for_primitive!(i32, I32);
impl_serde_schema_for_primitive!(i64, I64);
impl_serde_schema_for_primitive!(i128, I128);
// Serde data model has no isize/usize; serde serializes them via i64/u64.
impl_serde_schema_for_primitive!(isize, I64);
impl_serde_schema_for_primitive!(u8, U8);
impl_serde_schema_for_primitive!(u16, U16);
impl_serde_schema_for_primitive!(u32, U32);
impl_serde_schema_for_primitive!(u64, U64);
impl_serde_schema_for_primitive!(u128, U128);
impl_serde_schema_for_primitive!(usize, U64);
impl_serde_schema_for_primitive!(f32, F32);
impl_serde_schema_for_primitive!(f64, F64);
impl_serde_schema_for_primitive!(char, Char);

impl SerdeSchema for String {
    fn build_type_expr(_ctxt: &mut Context) -> TypeExpr {
        TypeExpr::String
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
                    elements: vec![$(<$T as SerdeSchema>::build_type_expr_no_recursion(ctxt)),+],
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
        TypeExpr::option(T::build_type_expr_no_recursion(ctxt))
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        T::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<T> SerdeSchema for Box<T>
where
    T: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        T::build_type_expr(ctxt)
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        T::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<T> SerdeSchema for Arc<T>
where
    T: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        T::build_type_expr(ctxt)
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        T::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<T> SerdeSchema for Rc<T>
where
    T: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        T::build_type_expr(ctxt)
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        T::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<T> SerdeSchema for Vec<T>
where
    T: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        let inner = T::build_type_expr_no_recursion(ctxt);
        if matches!(inner, TypeExpr::Primitive(PrimitiveType::U8)) {
            TypeExpr::Bytes
        } else {
            TypeExpr::seq(inner)
        }
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        T::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<K, V> SerdeSchema for HashMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        TypeExpr::map(
            K::build_type_expr_no_recursion(ctxt),
            V::build_type_expr_no_recursion(ctxt),
        )
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        K::on_register(registry);
        V::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}

impl<K, V> SerdeSchema for BTreeMap<K, V>
where
    K: SerdeSchema,
    V: SerdeSchema,
{
    fn build_type_expr(ctxt: &mut Context) -> TypeExpr {
        TypeExpr::map(
            K::build_type_expr_no_recursion(ctxt),
            V::build_type_expr_no_recursion(ctxt),
        )
    }

    fn on_register(registry: &mut RegistryContext) {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        K::on_register(registry);
        V::on_register(registry);
        registry.try_register::<Self>();
        registry.set_pending::<Self>(false);
    }
}
