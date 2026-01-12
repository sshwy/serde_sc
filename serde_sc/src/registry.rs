use crate::{SerdeSchema, expr::TypeExpr};
use std::{
    any::TypeId,
    collections::{HashMap, HashSet},
};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct TypeItem {
    pub expr: TypeExpr,
    pub name: &'static str,
}

/// The `Registry` struct holds a mapping from `TypeId` to `TypeExpr`, allowing fast lookup
/// of type information registered by the schema system.
#[derive(Debug, Clone)]
pub struct Registry {
    state: HashMap<TypeId, TypeItem>,
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}

impl Registry {
    /// Creates a new, empty `Registry`.
    pub fn new() -> Self {
        Self {
            state: HashMap::new(),
        }
    }

    fn try_register<T>(&mut self)
    where
        T: SerdeSchema + 'static,
    {
        let type_id = T::type_id();
        if self.state.contains_key(&type_id) {
            return;
        }
        self.state.insert(
            type_id,
            TypeItem {
                expr: T::type_expr(),
                name: std::any::type_name::<T>(),
            },
        );
    }

    /// Shorthand for calling `T::on_register(self)` for the given type `T`.
    pub fn register<T>(&mut self)
    where
        T: SerdeSchema + 'static,
    {
        T::on_register(&mut RegistryContext {
            registry: self,
            pending: HashSet::new(),
        });
    }

    /// Returns a reference to the `TypeExpr` associated with the given `type_id`, if it exists.
    pub fn get(&self, type_id: TypeId) -> Option<&TypeItem> {
        self.state.get(&type_id)
    }

    /// Returns an iterator over all registered `(TypeId, TypeExpr)` pairs.
    pub fn iter(&self) -> impl Iterator<Item = (&TypeId, &TypeItem)> {
        self.state.iter()
    }
}

/// `RegistryContext` provides contextual access to a `Registry`
/// and tracks currently pending type registrations to prevent cycles.
pub struct RegistryContext<'a> {
    registry: &'a mut Registry,
    pending: HashSet<TypeId>,
}

impl<'a> RegistryContext<'a> {
    /// Returns whether `T` is currently being registered (cycle breaker).
    pub fn is_pending<T: SerdeSchema>(&self) -> bool {
        self.pending.contains(&T::type_id())
    }

    /// Marks `T` as pending (true) or clears it (false).
    pub fn set_pending<T: SerdeSchema>(&mut self, pending: bool) {
        let type_id = T::type_id();
        if pending {
            self.pending.insert(type_id);
        } else {
            self.pending.remove(&type_id);
        }
    }

    pub fn try_register<T>(&mut self)
    where
        T: SerdeSchema,
    {
        self.registry.try_register::<T>();
    }
}
