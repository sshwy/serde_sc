use std::{any::TypeId, rc::Rc, sync::Arc};

use serde::Deserialize;
use serde_json::json;
use serde_sc::{SerdeSchema, registry::Registry};

use serde_sc_ts::{
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

#[derive(SerdeSchema, Debug, PartialEq, Eq)]
#[allow(dead_code)]
struct ArcFields {
    field: OptFields,
    arc: Arc<OptFields>,
    #[serde(flatten)]
    flatten_arc: Arc<OptFields>,
}

#[test]
fn test_arc_type() {
    let mut registry = Registry::new();
    registry.register::<OptFields>();
    registry.register::<Arc<OptFields>>();
    registry.register::<ArcFields>();
    registry.register::<Box<OptFields>>();
    let world = DeclWorld::new(&registry);
    eprintln!("{}", world.to_export_statements(Flavor::Serialize));
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
        "// Rust type: tests::MyStruct\nexport type MyStruct = {\n    id: number;\n};\n"
    );
}

#[derive(SerdeSchema)]
struct InnerOption;

#[derive(SerdeSchema)]
struct InnerVec;

#[derive(SerdeSchema)]
struct InnerArc;

#[derive(SerdeSchema)]
struct InnerRc;

#[derive(SerdeSchema)]
struct InnerBox;

#[test]
fn test_wrapper_types() {
    let mut registry = Registry::new();

    // Option
    registry.register::<Option<InnerOption>>();
    assert!(
        registry.get(TypeId::of::<InnerOption>()).is_some(),
        "dependent type should be registered"
    );

    // Vec
    registry.register::<Vec<InnerVec>>();
    assert!(
        registry.get(TypeId::of::<InnerVec>()).is_some(),
        "dependent type should be registered"
    );

    // Arc
    registry.register::<Arc<InnerArc>>();
    assert!(
        registry.get(TypeId::of::<InnerArc>()).is_some(),
        "dependent type should be registered for Arc"
    );

    // Rc
    registry.register::<Rc<InnerRc>>();
    assert!(
        registry.get(TypeId::of::<InnerRc>()).is_some(),
        "dependent type should be registered for Rc"
    );

    // Box
    registry.register::<Box<InnerBox>>();
    assert!(
        registry.get(TypeId::of::<InnerBox>()).is_some(),
        "dependent type should be registered for Box"
    );
}
