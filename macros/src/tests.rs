use quote::quote;
use syn::{Data, DeriveInput};

use crate::{parse_container_serde_attrs, struct_to_typeexpr};

#[test]
fn test_unit_struct() {
    let input: DeriveInput = syn::parse_str("struct Unit;").expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::UnitStruct {
            name: ::std::borrow::Cow::Borrowed("Unit")
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_primitive_fields_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: bool,
            b: i8,
            c: i16,
            d: i32,
            e: i64,
            f: i128,
            g: u8,
            h: u16,
            i: u32,
            j: u64,
            k: u128,
            l: f32,
            m: f64,
            n: char,
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::Bool)),
                ::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I8)),
                ::serde_schema::expr::Field::new("c", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I16)),
                ::serde_schema::expr::Field::new("d", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)),
                ::serde_schema::expr::Field::new("e", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I64)),
                ::serde_schema::expr::Field::new("f", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I128)),
                ::serde_schema::expr::Field::new("g", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)),
                ::serde_schema::expr::Field::new("h", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U16)),
                ::serde_schema::expr::Field::new("i", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)),
                ::serde_schema::expr::Field::new("j", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U64)),
                ::serde_schema::expr::Field::new("k", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U128)),
                ::serde_schema::expr::Field::new("l", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::F32)),
                ::serde_schema::expr::Field::new("m", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::F64)),
                ::serde_schema::expr::Field::new("n", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::Char))
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_string_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: String,
            b: &'static str,
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::String),
                ::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::String)
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_seq_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: Vec<i32>,
            b: Vec<u8>,
            c: [u8; 4],
            d: [i32; 3],
            e: &'static [i32],
            f: &'static [u8],
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)) }),
                ::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Bytes),
                ::serde_schema::expr::Field::new("c", ::serde_schema::expr::TypeExpr::Bytes),
                ::serde_schema::expr::Field::new("d", ::serde_schema::expr::TypeExpr::Tuple { elements: vec![::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32); 3usize] }),
                ::serde_schema::expr::Field::new("e", ::serde_schema::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)) }),
                ::serde_schema::expr::Field::new("f", ::serde_schema::expr::TypeExpr::Bytes)
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_map_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: std::collections::HashMap<String, u8>,
            b: std::collections::BTreeMap<u32, Vec<u8>>,
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::String), value: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)), }),
                ::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)), value: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Bytes), })
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_unit_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: (),
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Unit)
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_option_in_struct() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            a: Option<u32>,
            b: Option<Vec<u8>>,
        }
        "#,
    )
    .expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: vec![
                ::serde_schema::expr::Field::new(
                    "a",
                    ::serde_schema::expr::TypeExpr::Option(
                        ::std::boxed::Box::new(
                            ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)
                        )
                    )
                ),
                ::serde_schema::expr::Field::new(
                    "b",
                    ::serde_schema::expr::TypeExpr::Option(
                        ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Bytes)
                    )
                )
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_newtype_struct() {
    let input: DeriveInput = syn::parse_str("struct New(u8);").expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::NewtypeStruct {
            name: ::std::borrow::Cow::Borrowed("New"),
            inner: ::std::boxed::Box::new(
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
            ),
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_tuple_struct() {
    let input: DeriveInput = syn::parse_str("struct Tup(u8, i32);").expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::TupleStruct {
            name: ::std::borrow::Cow::Borrowed("Tup"),
            elements: vec![
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8),
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)
            ],
        }
    };

    assert_eq!(got.to_string(), expected.to_string());
}
