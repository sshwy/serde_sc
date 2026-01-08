use crate::{SerdeSchema, expr::TypeExpr};
use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, RwLock},
};

#[derive(Debug, Default)]
struct TypeCollection {
    state: HashMap<std::any::TypeId, Arc<TypeExpr>>,
}

static GLOBAL: LazyLock<RwLock<TypeCollection>> = LazyLock::new(|| RwLock::default());

/// Registers a type `T` by inserting its schema representation into the global type collection.
pub fn register<T>()
where
    T: SerdeSchema + 'static,
{
    let type_expr = T::serde_sc();
    let type_id = std::any::TypeId::of::<T>();
    GLOBAL
        .write()
        .unwrap()
        .state
        .insert(type_id, Arc::new(type_expr));
}

/// Retrieves the type expression associated with the given `TypeId` from the global type collection.
pub fn get(type_id: std::any::TypeId) -> Option<Arc<TypeExpr>> {
    GLOBAL.read().unwrap().state.get(&type_id).cloned()
}
