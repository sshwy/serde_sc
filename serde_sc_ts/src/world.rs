use std::{
    any::TypeId,
    collections::{BTreeMap, BTreeSet, HashMap},
};

use crate::{
    expr::{ArrayType, Field, RecordType, StructType, TupleType, TypeExpr},
    value,
};
use serde_sc::{expr::TypeExpr as ScTypeExpr, registry::Registry};

pub struct DeclWorld<'a> {
    registry: &'a Registry,
    id_to_name: HashMap<TypeId, (Option<String>, &'static str)>,
    name_to_id: BTreeMap<String, (TypeId, &'static str)>,
}

impl<'a> DeclWorld<'a> {
    pub fn new(registry: &'a Registry) -> Self {
        // Deterministic output order (registry is backed by a HashMap).
        let mut id_to_name = HashMap::new();
        let mut name_to_id = BTreeMap::new();
        for (type_id, item) in registry.iter() {
            let name = item.expr.name();
            if let Some(name) = name {
                let name: &str = if name_to_id.contains_key(name) {
                    &(String::from(name) + "_")
                } else {
                    name
                };
                name_to_id.insert(name.to_string(), (*type_id, item.name));
            }
            id_to_name.insert(*type_id, (name.map(|s| s.to_string()), item.name));
        }
        Self {
            registry,
            id_to_name,
            name_to_id,
        }
    }

    /// Converts the given Rust type (by TypeId) into a TypeScript TypeExpr using the configured flavor.
    pub fn to_type_expr(&self, type_id: TypeId, flavor: Flavor) -> TypeExpr {
        let item = self
            .registry
            .get(type_id)
            .expect("type not found in registry");
        to_ts_type_expr(&item.expr, self, flavor)
    }

    /// Generates a TypeScript export statement for the given type, including a comment describing the Rust type name.
    pub fn to_export_statement(&self, type_id: TypeId, flavor: Flavor) -> String {
        let (name, rust_name) = self.resolve(type_id).expect("type name not found");

        let ts_expr = self.to_type_expr(type_id, flavor);
        let ts_expr_str = format!("{:80.4}", ts_expr);

        let mut out = String::new();
        out.push_str(&format!("// Rust type: {}\n", rust_name));
        out.push_str("export type ");
        out.push_str(&name);
        out.push_str(" = ");
        out.push_str(&ts_expr_str);
        out.push_str(";\n");
        out
    }

    /// Returns an iterator over all registered type ids in the lexical order of the type names.
    pub fn iter_type_ids(&self) -> impl Iterator<Item = (TypeId, &'static str)> {
        self.name_to_id.values().copied()
    }

    /// Generates TypeScript export statements for all registered types.
    pub fn to_export_statements(&self, flavor: Flavor) -> String {
        let mut out = String::new();
        for (_, item) in &self.name_to_id {
            out.push_str(&self.to_export_statement(item.0, flavor));
        }
        out
    }

    /// Returns the Rust type name for the given TypeId, if it exists.
    pub fn resolve(&self, type_id: TypeId) -> Option<(String, &'static str)> {
        if let Some(s) = self.resolve_primitives(type_id) {
            return Some(s);
        }

        if let Some((name, rust_name)) = self.id_to_name.get(&type_id).cloned() {
            let Some(name) = name else {
                panic!("name not found for type {type_id:?} ({rust_name})");
            };
            return Some((name, rust_name));
        }

        None
    }

    fn resolve_primitives(&self, type_id: TypeId) -> Option<(String, &'static str)> {
        macro_rules! if_match {
            ($type_id:ident, $name:literal, $( $ty:ty )|* ) => {
                $( if $type_id == TypeId::of::<$ty>() {
                    return Some(($name.to_string(), std::any::type_name::<$ty>()));
                } )*
            };
        }
        if_match!(type_id, "number", i8 | i16 | i32 | i64 | i128 | isize);
        if_match!(type_id, "number", u8 | u16 | u32 | u64 | u128 | usize);
        if_match!(type_id, "number", f32 | f64);
        if_match!(type_id, "boolean", bool);
        if_match!(type_id, "string", String);
        None
    }
}

#[derive(Clone, Copy)]
pub enum Flavor {
    Serialize,
    Deserialize,
}

fn to_ts_type_field(expr: &ScTypeExpr, world: &DeclWorld, flavor: Flavor) -> Field {
    match flavor {
        Flavor::Serialize => Field::new(to_ts_type_expr(expr, world, flavor), false),
        Flavor::Deserialize => match expr {
            ScTypeExpr::Option(type_expr) => {
                let mut type_expr: &ScTypeExpr = type_expr.as_ref();
                // unwrap nested options
                while let ScTypeExpr::Option(inner) = type_expr {
                    type_expr = inner;
                }
                Field::new(to_ts_type_expr(type_expr, world, flavor), true)
            }
            _ => Field::new(to_ts_type_expr(expr, world, flavor), false),
        },
    }
}

fn to_ts_type_expr(expr: &ScTypeExpr, world: &DeclWorld, flavor: Flavor) -> TypeExpr {
    fn ts_str_lit(s: &str) -> TypeExpr {
        TypeExpr::Value(value::Value::String(s.to_owned()))
    }

    fn to_struct_type_expr(items: impl IntoIterator<Item = (String, Field)>) -> TypeExpr {
        let record = items.into_iter().collect::<BTreeMap<String, Field>>();
        TypeExpr::Struct(StructType::new(record))
    }

    match expr {
        ScTypeExpr::Remote { path, type_id } => {
            let Some(name) = world.resolve(*type_id) else {
                let e = world.registry.get(*type_id).map(|o| o.name);
                panic!("failed to get name of remote type {path} ({e:?})")
            };
            TypeExpr::Remote(name.0)
        }

        ScTypeExpr::Primitive(p) => match p {
            serde_sc::expr::PrimitiveType::Bool => TypeExpr::Boolean,
            serde_sc::expr::PrimitiveType::Char => TypeExpr::String,
            serde_sc::expr::PrimitiveType::I8
            | serde_sc::expr::PrimitiveType::I16
            | serde_sc::expr::PrimitiveType::I32
            | serde_sc::expr::PrimitiveType::I64
            | serde_sc::expr::PrimitiveType::I128
            | serde_sc::expr::PrimitiveType::U8
            | serde_sc::expr::PrimitiveType::U16
            | serde_sc::expr::PrimitiveType::U32
            | serde_sc::expr::PrimitiveType::U64
            | serde_sc::expr::PrimitiveType::U128
            | serde_sc::expr::PrimitiveType::F32
            | serde_sc::expr::PrimitiveType::F64 => TypeExpr::Number,
        },

        ScTypeExpr::String => TypeExpr::String,
        ScTypeExpr::Bytes => TypeExpr::Array(ArrayType::new(TypeExpr::Number)),

        ScTypeExpr::Option(inner) => TypeExpr::Union(
            [
                to_ts_type_expr(inner.as_ref(), world, flavor),
                TypeExpr::null(),
            ]
            .into(),
        ),

        ScTypeExpr::Unit => TypeExpr::null(),
        ScTypeExpr::UnitStruct { .. } => TypeExpr::null(),

        ScTypeExpr::NewtypeStruct { inner, .. } => to_ts_type_expr(inner.as_ref(), world, flavor),

        ScTypeExpr::Seq { element } => TypeExpr::Array(ArrayType::new(to_ts_type_expr(
            element.as_ref(),
            world,
            flavor,
        ))),

        ScTypeExpr::Tuple { elements } => TypeExpr::Tuple(TupleType::new(
            elements
                .iter()
                .map(|e| to_ts_type_expr(e, world, flavor))
                .collect(),
        )),

        ScTypeExpr::TupleStruct { elements, .. } => TypeExpr::Tuple(TupleType::new(
            elements
                .iter()
                .map(|e| to_ts_type_expr(e, world, flavor))
                .collect(),
        )),

        ScTypeExpr::Map { key, value: val } => TypeExpr::Record(RecordType::new(
            to_ts_type_expr(key.as_ref(), world, flavor),
            to_ts_type_expr(val.as_ref(), world, flavor),
        )),

        ScTypeExpr::Struct { fields, .. } => to_struct_type_expr(fields.iter().map(|f| {
            (
                f.name.as_ref().to_string(),
                to_ts_type_field(&f.ty, world, flavor),
            )
        })),

        ScTypeExpr::Enum {
            tag,
            content,
            variants,
            ..
        } => {
            let mut items: BTreeSet<TypeExpr> = BTreeSet::new();

            for v in variants {
                let vname = v.name.as_ref();

                let item = match (tag.as_deref(), content.as_deref(), &v.kind) {
                    // Externally tagged (default serde representation)
                    (None, None, serde_sc::expr::VariantKind::Unit) => ts_str_lit(vname),
                    (None, None, serde_sc::expr::VariantKind::Newtype(inner)) => {
                        to_struct_type_expr([(
                            vname.to_string(),
                            to_ts_type_field(inner.as_ref(), world, flavor),
                        )])
                    }
                    (None, None, serde_sc::expr::VariantKind::Tuple(elements)) => {
                        let tup = TypeExpr::Tuple(TupleType::new(
                            elements
                                .iter()
                                .map(|e| to_ts_type_expr(e, world, flavor))
                                .collect(),
                        ));
                        to_struct_type_expr([(vname.to_string(), Field::new(tup, false))])
                    }
                    (None, None, serde_sc::expr::VariantKind::Struct(fields)) => {
                        let inner = to_struct_type_expr(fields.iter().map(|f| {
                            (
                                f.name.as_ref().to_string(),
                                to_ts_type_field(&f.ty, world, flavor),
                            )
                        }));
                        to_struct_type_expr([(vname.to_string(), Field::new(inner, false))])
                    }

                    // Internally tagged: `#[serde(tag = "...")]`
                    (Some(tag_key), None, serde_sc::expr::VariantKind::Unit) => {
                        to_struct_type_expr([(
                            tag_key.to_string(),
                            Field::new(ts_str_lit(vname), false),
                        )])
                    }
                    (Some(tag_key), None, serde_sc::expr::VariantKind::Struct(fields)) => {
                        let mut record: BTreeMap<String, Field> = BTreeMap::new();
                        record.insert(tag_key.to_string(), Field::new(ts_str_lit(vname), false));
                        for f in fields {
                            record.insert(
                                f.name.as_ref().to_string(),
                                to_ts_type_field(&f.ty, world, flavor),
                            );
                        }
                        TypeExpr::Struct(StructType::new(record))
                    }

                    // Adjacently tagged: `#[serde(tag = "...", content = "...")]`
                    (Some(tag_key), Some(_content_key), serde_sc::expr::VariantKind::Unit) => {
                        // Unit variant has no content field in serde's adjacently tagged representation.
                        to_struct_type_expr([(
                            tag_key.to_string(),
                            Field::new(ts_str_lit(vname), false),
                        )])
                    }
                    (
                        Some(tag_key),
                        Some(content_key),
                        serde_sc::expr::VariantKind::Newtype(inner),
                    ) => to_struct_type_expr([
                        (tag_key.to_string(), Field::new(ts_str_lit(vname), false)),
                        (
                            content_key.to_string(),
                            to_ts_type_field(inner.as_ref(), world, flavor),
                        ),
                    ]),
                    (
                        Some(tag_key),
                        Some(content_key),
                        serde_sc::expr::VariantKind::Tuple(elements),
                    ) => {
                        let tup = TypeExpr::Tuple(TupleType::new(
                            elements
                                .iter()
                                .map(|e| to_ts_type_expr(e, world, flavor))
                                .collect(),
                        ));
                        to_struct_type_expr([
                            (tag_key.to_string(), Field::new(ts_str_lit(vname), false)),
                            (content_key.to_string(), Field::new(tup, false)),
                        ])
                    }
                    (
                        Some(tag_key),
                        Some(content_key),
                        serde_sc::expr::VariantKind::Struct(fields),
                    ) => {
                        let inner = to_struct_type_expr(fields.iter().map(|f| {
                            (
                                f.name.as_ref().to_string(),
                                to_ts_type_field(&f.ty, world, flavor),
                            )
                        }));
                        to_struct_type_expr([
                            (tag_key.to_string(), Field::new(ts_str_lit(vname), false)),
                            (content_key.to_string(), Field::new(inner, false)),
                        ])
                    }

                    // Unsupported / unexpected combinations (e.g. untagged, or internal tagging with tuple/newtype).
                    _ => TypeExpr::Any,
                };

                items.insert(item);
            }

            TypeExpr::Union(items.into())
        }
    }
}
