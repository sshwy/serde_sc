use serde_json::json;
use serde_sc::SerdeSchema;

#[derive(serde::Deserialize, serde_sc::SerdeSchema)]
struct TestDe {
    number: f64,
    string: String,
    boolean: bool,
    array: Vec<i32>,
    option: Option<String>,
}

#[test]
fn test_deserialize() {
    let data: TestDe = serde_json::from_value(json!({
        "number": 1.0,
        "string": "test",
        "boolean": true,
        "array": [1, 2, 3],
    }))
    .unwrap();
}
