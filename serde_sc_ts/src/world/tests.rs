use std::{any::TypeId, sync::Arc};

use serde::Deserialize;
use serde_json::json;
use serde_sc::{SerdeSchema, registry::Registry};

use crate::{
    DeclWorld, Flavor,
    expr::{Field, TypeExpr},
};

#[derive(SerdeSchema, Deserialize, Debug, PartialEq, Eq)]
#[allow(dead_code)]
struct OptFields {
    opt_field: Option<i32>,
    opt_opt_field: Option<Option<i32>>,
    opt_opt_opt_field: Option<Option<Option<i32>>>,
}

#[derive(SerdeSchema, Debug, PartialEq, Eq)]
#[allow(dead_code)]
struct ArcFields {
    #[serde(flatten)]
    arc: Arc<OptFields>,
}

// Assertions tell us that nested options are handled just like a single option during JSON deserialization.
#[test]
fn test_de_option() {
    assert_eq!(
        OptFields {
            opt_field: None,
            opt_opt_field: None,
            opt_opt_opt_field: None,
        },
        serde_json::from_value(json!({})).unwrap()
    );

    assert_eq!(
        OptFields {
            opt_field: None,
            opt_opt_field: None,
            opt_opt_opt_field: None,
        },
        serde_json::from_value(json!({
            "opt_field": null,
            "opt_opt_field": null,
            "opt_opt_opt_field": null,
        }))
        .unwrap()
    );

    assert_eq!(
        OptFields {
            opt_field: Some(1),
            opt_opt_field: Some(Some(1)),
            opt_opt_opt_field: Some(Some(Some(1))),
        },
        serde_json::from_value(json!({
            "opt_field": 1,
            "opt_opt_field": 1,
            "opt_opt_opt_field": 1,
        }))
        .unwrap()
    );

    let mut registry = Registry::new();
    registry.register::<OptFields>();
    let world = DeclWorld::new(&registry);
    let type_id = TypeId::of::<OptFields>();
    let ts = world.to_type_expr(type_id, Flavor::Deserialize);
    assert_eq!(
        ts,
        TypeExpr::struct_type([
            (
                String::from("opt_field"),
                Field::new(TypeExpr::Number, true)
            ),
            (
                String::from("opt_opt_field"),
                Field::new(TypeExpr::Number, true)
            ),
            (
                String::from("opt_opt_opt_field"),
                Field::new(TypeExpr::Number, true)
            ),
        ])
    );
    println!(
        "{}",
        world.to_export_statement(type_id, Flavor::Deserialize)
    )
}

#[test]
fn test_arc_type() {
    let mut registry = Registry::new();
    registry.register::<OptFields>();
    registry.register::<Arc<OptFields>>();
    registry.register::<ArcFields>();
    registry.register::<Box<OptFields>>();
    let world = DeclWorld::new(&registry);
    world.to_export_statements(Flavor::Serialize);
}

#[test]
fn test_arc_flatten() {
    let mut registry = Registry::new();
    registry.register::<ArcFields>();
    let world = DeclWorld::new(&registry);
    dbg!(world.to_export_statements(Flavor::Serialize));
}

type Id = i32;

#[derive(SerdeSchema, Debug)]
#[allow(dead_code)]
struct MyStruct {
    id: Id,
}

#[test]
fn test_primitive_fields() {
    let mut registry = Registry::new();
    registry.register::<MyStruct>();
    let world = DeclWorld::new(&registry);
    assert_eq!(
        world.to_export_statements(Flavor::Serialize),
        "// Rust type: MyStruct\nexport type MyStruct = {\n    id: number;\n};\n"
    );
}
