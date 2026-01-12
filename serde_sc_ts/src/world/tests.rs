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
struct TestDe {
    opt_field: Option<i32>,
    opt_opt_field: Option<Option<i32>>,
    opt_opt_opt_field: Option<Option<Option<i32>>>,
}

// Assertions tell us that nested options are handled just like a single option during JSON deserialization.
#[test]
fn test_de_option() {
    assert_eq!(
        TestDe {
            opt_field: None,
            opt_opt_field: None,
            opt_opt_opt_field: None,
        },
        serde_json::from_value(json!({})).unwrap()
    );

    assert_eq!(
        TestDe {
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
        TestDe {
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
    registry.register::<TestDe>();
    let world = DeclWorld::new(&registry);
    let type_id = TypeId::of::<TestDe>();
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
    registry.register::<TestDe>();
    registry.register::<Arc<TestDe>>();
    let world = DeclWorld::new(&registry);
    world.to_export_statements(Flavor::Serialize);
}
