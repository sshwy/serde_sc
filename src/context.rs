use std::{any::TypeId, collections::HashSet};

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
