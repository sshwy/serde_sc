// use serde_sc::registry::Registry;

use std::{
    any::TypeId,
    collections::{BTreeMap, BTreeSet},
};

use crate::{
    expr::{ArrayType, Field, RecordType, StructType, TupleType, TypeExpr},
    value,
};
use serde_sc::{expr::TypeExpr as ScTypeExpr, registry::Registry};

struct NameResolver<'a> {
    registry: &'a Registry,
}

impl<'a> NameResolver<'a> {
    fn resolve(&self, type_id: TypeId) -> Option<String> {
        self.registry
            .get(type_id)
            .and_then(|expr| expr.name().map(|name| name.to_string()))
    }
}

fn to_ts_type_expr(expr: &ScTypeExpr, name_resolver: &NameResolver) -> TypeExpr {
    fn ts_str_lit(s: &str) -> TypeExpr {
        TypeExpr::Value(value::Value::String(s.to_owned()))
    }

    fn to_struct_type_expr(items: impl IntoIterator<Item = (String, TypeExpr)>) -> TypeExpr {
        let record = items
            .into_iter()
            .map(|(k, v)| (k, Field::new(v)))
            .collect::<BTreeMap<String, Field>>();
        TypeExpr::Struct(StructType::new(record))
    }

    match expr {
        ScTypeExpr::Remote { path: _, type_id } => {
            let name = name_resolver.resolve(*type_id).unwrap();
            TypeExpr::Remote(name)
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
                to_ts_type_expr(inner.as_ref(), name_resolver),
                TypeExpr::null(),
            ]
            .into(),
        ),

        ScTypeExpr::Unit => TypeExpr::null(),
        ScTypeExpr::UnitStruct { .. } => TypeExpr::null(),

        ScTypeExpr::NewtypeStruct { inner, .. } => to_ts_type_expr(inner.as_ref(), name_resolver),

        ScTypeExpr::Seq { element } => TypeExpr::Array(ArrayType::new(to_ts_type_expr(
            element.as_ref(),
            name_resolver,
        ))),

        ScTypeExpr::Tuple { elements } => TypeExpr::Tuple(TupleType::new(
            elements
                .iter()
                .map(|e| to_ts_type_expr(e, name_resolver))
                .collect(),
        )),

        ScTypeExpr::TupleStruct { elements, .. } => TypeExpr::Tuple(TupleType::new(
            elements
                .iter()
                .map(|e| to_ts_type_expr(e, name_resolver))
                .collect(),
        )),

        ScTypeExpr::Map { key, value: val } => TypeExpr::Record(RecordType::new(
            to_ts_type_expr(key.as_ref(), name_resolver),
            to_ts_type_expr(val.as_ref(), name_resolver),
        )),

        ScTypeExpr::Struct { fields, .. } => to_struct_type_expr(fields.iter().map(|f| {
            (
                f.name.as_ref().to_string(),
                to_ts_type_expr(&f.ty, name_resolver),
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
                            to_ts_type_expr(inner.as_ref(), name_resolver),
                        )])
                    }
                    (None, None, serde_sc::expr::VariantKind::Tuple(elements)) => {
                        let tup = TypeExpr::Tuple(TupleType::new(
                            elements
                                .iter()
                                .map(|e| to_ts_type_expr(e, name_resolver))
                                .collect(),
                        ));
                        to_struct_type_expr([(vname.to_string(), tup)])
                    }
                    (None, None, serde_sc::expr::VariantKind::Struct(fields)) => {
                        let inner = to_struct_type_expr(fields.iter().map(|f| {
                            (
                                f.name.as_ref().to_string(),
                                to_ts_type_expr(&f.ty, name_resolver),
                            )
                        }));
                        to_struct_type_expr([(vname.to_string(), inner)])
                    }

                    // Internally tagged: `#[serde(tag = "...")]`
                    (Some(tag_key), None, serde_sc::expr::VariantKind::Unit) => {
                        to_struct_type_expr([(tag_key.to_string(), ts_str_lit(vname))])
                    }
                    (Some(tag_key), None, serde_sc::expr::VariantKind::Struct(fields)) => {
                        let mut record: BTreeMap<String, Field> = BTreeMap::new();
                        record.insert(tag_key.to_string(), ts_str_lit(vname).into());
                        for f in fields {
                            record.insert(
                                f.name.as_ref().to_string(),
                                to_ts_type_expr(&f.ty, name_resolver).into(),
                            );
                        }
                        TypeExpr::Struct(StructType::new(record))
                    }

                    // Adjacently tagged: `#[serde(tag = "...", content = "...")]`
                    (Some(tag_key), Some(_content_key), serde_sc::expr::VariantKind::Unit) => {
                        // Unit variant has no content field in serde's adjacently tagged representation.
                        to_struct_type_expr([(tag_key.to_string(), ts_str_lit(vname))])
                    }
                    (
                        Some(tag_key),
                        Some(content_key),
                        serde_sc::expr::VariantKind::Newtype(inner),
                    ) => to_struct_type_expr([
                        (tag_key.to_string(), ts_str_lit(vname)),
                        (
                            content_key.to_string(),
                            to_ts_type_expr(inner.as_ref(), name_resolver),
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
                                .map(|e| to_ts_type_expr(e, name_resolver))
                                .collect(),
                        ));
                        to_struct_type_expr([
                            (tag_key.to_string(), ts_str_lit(vname)),
                            (content_key.to_string(), tup),
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
                                to_ts_type_expr(&f.ty, name_resolver),
                            )
                        }));
                        to_struct_type_expr([
                            (tag_key.to_string(), ts_str_lit(vname)),
                            (content_key.to_string(), inner),
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

/// Export all types in the registry to TypeScript type declaration statement.
pub fn to_typescript(registry: &Registry) -> String {
    let name_resolver = NameResolver { registry };

    // Deterministic output order (registry is backed by a HashMap).
    let mut named: BTreeMap<String, &ScTypeExpr> = BTreeMap::new();
    for (_type_id, expr) in registry.iter() {
        if let Some(name) = expr.name() {
            named.insert(name.to_string(), expr);
        }
    }

    let mut out = String::new();
    for (name, expr) in named {
        let ts_expr = to_ts_type_expr(expr, &name_resolver);
        let ts_expr_str = format!("{:80.4}", ts_expr);
        out.push_str("export type ");
        out.push_str(&name);
        out.push_str(" = ");
        out.push_str(&ts_expr_str);
        out.push_str(";\n");
    }

    out
}
