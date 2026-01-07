//! Serde data model type expressions.
//!
//! Based on: <https://serde.rs/data-model.html>
//!
//! This module defines a small AST ("type expression") that can represent the 29
//! Serde data model types (plus the necessary structure for named fields and
//! enum variants).

/// Primitive types in the Serde data model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
}

/// A (named) field in a `struct` / `struct_variant`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub name: String,
    pub ty: TypeExpr,
}

impl Field {
    pub fn new(name: impl Into<String>, ty: impl Into<TypeExpr>) -> Self {
        Self {
            name: name.into(),
            ty: ty.into(),
        }
    }
}

/// An enum variant in the Serde data model.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: String,
    pub kind: VariantKind,
}

impl EnumVariant {
    pub fn new(name: impl Into<String>, kind: VariantKind) -> Self {
        Self {
            name: name.into(),
            kind,
        }
    }
}

/// The shape of an enum variant: unit / newtype / tuple / struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariantKind {
    /// `unit_variant` (e.g. `E::A`)
    Unit,
    /// `newtype_variant` (e.g. `E::N(u8)`)
    Newtype(Box<TypeExpr>),
    /// `tuple_variant` (e.g. `E::T(u8, u8)`)
    Tuple(Vec<TypeExpr>),
    /// `struct_variant` (e.g. `E::S { r: u8, g: u8 }`)
    Struct(Vec<Field>),
}

/// A type expression capable of representing the Serde data model types.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    /// One of the 14 primitive types.
    Primitive(PrimitiveType),

    /// Serde `string` (UTF-8 bytes, may contain 0-bytes).
    String,

    /// Serde `byte array` (`[u8]`).
    Bytes,

    /// Serde `option`.
    Option(Box<TypeExpr>),

    /// Serde `unit` (`()`).
    Unit,

    /// Serde `unit_struct` (named value containing no data).
    UnitStruct { name: String },

    /// Serde `newtype_struct` (named wrapper around a single value).
    NewtypeStruct { name: String, inner: Box<TypeExpr> },

    /// Serde `seq` (variably-sized heterogeneous sequence).
    Seq { element: Box<TypeExpr> },

    /// Serde `tuple` (statically-sized heterogeneous sequence).
    Tuple { elements: Vec<TypeExpr> },

    /// Serde `tuple_struct` (named tuple).
    TupleStruct {
        name: String,
        elements: Vec<TypeExpr>,
    },

    /// Serde `map` (variably-sized heterogeneous key-value pairing).
    Map {
        key: Box<TypeExpr>,
        value: Box<TypeExpr>,
    },

    /// Serde `struct` (statically-sized key-value pairing with string keys).
    Struct {
        name: Option<String>,
        fields: Vec<Field>,
    },

    /// Serde enum representation (covers `unit_variant` / `newtype_variant` /
    /// `tuple_variant` / `struct_variant`).
    Enum {
        name: Option<String>,
        variants: Vec<EnumVariant>,
    },
}

impl TypeExpr {
    pub fn option(inner: impl Into<TypeExpr>) -> Self {
        Self::Option(Box::new(inner.into()))
    }

    pub fn seq(element: impl Into<TypeExpr>) -> Self {
        Self::Seq {
            element: Box::new(element.into()),
        }
    }

    pub fn map(key: impl Into<TypeExpr>, value: impl Into<TypeExpr>) -> Self {
        Self::Map {
            key: Box::new(key.into()),
            value: Box::new(value.into()),
        }
    }
}

impl From<PrimitiveType> for TypeExpr {
    fn from(value: PrimitiveType) -> Self {
        Self::Primitive(value)
    }
}
