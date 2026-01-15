use std::{any::TypeId, collections::HashMap};

/// Context used during schema building, such as for tracking which types are being processed to prevent infinite recursion.
#[derive(Debug, Default)]
pub struct Context {
    pending: HashMap<TypeId, &'static str>,
}

impl Context {
    /// Returns `true` if the type `T` is currently pending in this context.
    pub fn is_pending<T>(&self) -> bool
    where
        T: 'static,
    {
        self.pending.contains_key(&TypeId::of::<T>())
    }

    /// Marks the type `T` as pending or not, according to the `pending` argument.
    pub fn set_pending<T>(&mut self, pending: bool)
    where
        T: 'static,
    {
        let id = TypeId::of::<T>();
        if pending {
            self.pending.insert(id, std::any::type_name::<T>());
        } else {
            self.pending.remove(&id);
        }
    }

    #[doc(hidden)]
    pub fn pending(&self) -> &HashMap<TypeId, &'static str> {
        &self.pending
    }
}
