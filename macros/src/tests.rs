use quote::quote;
use syn::{Data, DeriveInput};

use crate::{
    enum_to_typeexpr, expand_serde_schema, parse_container_serde_attrs, struct_to_typeexpr,
};

fn check_struct_to_typeexpr(input: &str, expected: proc_macro2::TokenStream) {
    let input: DeriveInput = syn::parse_str(input).expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container).expect("struct_to_typeexpr");
    assert_eq!(got.to_string(), expected.to_string());
}

fn check_enum_to_typeexpr(input: &str, expected: proc_macro2::TokenStream) {
    let input: DeriveInput = syn::parse_str(input).expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");

    let de = match &input.data {
        Data::Enum(de) => de,
        _ => panic!("expected enum"),
    };

    let got = enum_to_typeexpr(&input.ident, de, &container).expect("enum_to_typeexpr");
    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_unit_struct() {
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::UnitStruct {
            name: ::std::borrow::Cow::Borrowed("Unit")
        }
    };
    check_struct_to_typeexpr("struct Unit;", expected);
}

#[test]
fn test_primitive_fields_in_struct() {
    let input = r#"
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
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::Bool)));
        __fields.push(::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I8)));
        __fields.push(::serde_schema::expr::Field::new("c", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I16)));
        __fields.push(::serde_schema::expr::Field::new("d", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)));
        __fields.push(::serde_schema::expr::Field::new("e", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I64)));
        __fields.push(::serde_schema::expr::Field::new("f", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I128)));
        __fields.push(::serde_schema::expr::Field::new("g", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)));
        __fields.push(::serde_schema::expr::Field::new("h", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U16)));
        __fields.push(::serde_schema::expr::Field::new("i", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)));
        __fields.push(::serde_schema::expr::Field::new("j", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U64)));
        __fields.push(::serde_schema::expr::Field::new("k", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U128)));
        __fields.push(::serde_schema::expr::Field::new("l", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::F32)));
        __fields.push(::serde_schema::expr::Field::new("m", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::F64)));
        __fields.push(::serde_schema::expr::Field::new("n", ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::Char)));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_string_in_struct() {
    let input = r#"
        struct S {
            a: String,
            b: &'static str,
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::String));
        __fields.push(::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::String));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_seq_in_struct() {
    let input = r#"
        struct S {
            a: Vec<i32>,
            b: Vec<u8>,
            c: [u8; 4],
            d: [i32; 3],
            e: &'static [i32],
            f: &'static [u8],
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)) }));
        __fields.push(::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Bytes));
        __fields.push(::serde_schema::expr::Field::new("c", ::serde_schema::expr::TypeExpr::Bytes));
        __fields.push(::serde_schema::expr::Field::new("d", ::serde_schema::expr::TypeExpr::Tuple { elements: vec![::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32); 3usize] }));
        __fields.push(::serde_schema::expr::Field::new("e", ::serde_schema::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)) }));
        __fields.push(::serde_schema::expr::Field::new("f", ::serde_schema::expr::TypeExpr::Bytes));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_map_in_struct() {
    let input = r#"
        struct S {
            a: std::collections::HashMap<String, u8>,
            b: std::collections::BTreeMap<u32, Vec<u8>>,
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::String), value: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)), }));
        __fields.push(::serde_schema::expr::Field::new("b", ::serde_schema::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)), value: ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Bytes), }));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_unit_in_struct() {
    let input = r#"
        struct S {
            a: (),
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new("a", ::serde_schema::expr::TypeExpr::Unit));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_option_in_struct() {
    let input = r#"
        struct S {
            a: Option<u32>,
            b: Option<Vec<u8>>,
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new(
            "a",
            ::serde_schema::expr::TypeExpr::Option(
                ::std::boxed::Box::new(
                    ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)
                )
            )
        ));
        __fields.push(::serde_schema::expr::Field::new(
            "b",
            ::serde_schema::expr::TypeExpr::Option(
                ::std::boxed::Box::new(::serde_schema::expr::TypeExpr::Bytes)
            )
        ));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_newtype_struct() {
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::NewtypeStruct {
            name: ::std::borrow::Cow::Borrowed("New"),
            inner: ::std::boxed::Box::new(
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
            ),
        }
    };
    check_struct_to_typeexpr("struct New(u8);", expected);
}

#[test]
fn test_tuple_struct() {
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::TupleStruct {
            name: ::std::borrow::Cow::Borrowed("Tup"),
            elements: vec![
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8),
                ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)
            ],
        }
    };
    check_struct_to_typeexpr("struct Tup(u8, i32);", expected);
}

#[test]
fn test_struct_rename_attr() {
    let input = r#"
        #[serde(rename = "Renamed")]
        struct S {
            a: u8,
        }
    "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_schema::expr::Field::new(
            "a",
            ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
        ));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("Renamed"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_newtype_struct_transparent_attr() {
    let input = r#"
        #[serde(transparent)]
        struct Wrap(u32);
    "#;
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)
    };
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_braced_struct_transparent_attr() {
    let input = r#"
        #[serde(transparent)]
        struct Wrap {
            a: u32,
        }
    "#;
    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U32)
    };
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_struct_flatten_attr() {
    let input = r#"
        struct Outer {
            #[serde(flatten)]
            inner: Inner,
            a: u8,
        }
    "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
        {
            let __inner = <Inner as ::serde_schema::SerdeSchema>::serde_schema();
            match __inner {
                ::serde_schema::expr::TypeExpr::Struct { name: _, fields } => {
                    __fields.extend(fields);
                }
                _other => {
                    panic!(
                        "serde_schema: #[serde(flatten)] is only supported for fields whose SerdeSchema is TypeExpr::Struct"
                    );
                }
            }
        }
        __fields.push(::serde_schema::expr::Field::new(
            "a",
            ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
        ));
        ::serde_schema::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("Outer"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_struct_tag_attr_errors() {
    let input: DeriveInput = syn::parse_str(
        r#"
        #[serde(tag = "type")]
        struct S {
            a: u8,
        }
        "#,
    )
    .expect("parse input");

    let err = expand_serde_schema(&input).expect_err("expected error");
    let msg = err.to_string();
    assert!(msg.contains("tag"), "msg was: {msg}");
}

#[test]
fn test_enum_untagged_attr_errors() {
    let input: DeriveInput = syn::parse_str(
        r#"
        #[serde(untagged)]
        enum E {
            A { a: u8 },
            B { b: u8 },
        }
        "#,
    )
    .expect("parse input");

    let err = expand_serde_schema(&input).expect_err("expected error");
    let msg = err.to_string();
    assert!(msg.contains("untagged"), "msg was: {msg}");
}

#[test]
fn test_enum_tag_attr() {
    let input = r#"
        #[serde(tag = "type")]
        enum E {
            A { a: u8 },
            B,
        }
    "#;

    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::Some(::std::borrow::Cow::Borrowed("type")),
            variants: vec![
                ::serde_schema::expr::EnumVariant::new("A", {
                    let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
                    __fields.push(::serde_schema::expr::Field::new(
                        "a",
                        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
                    ));
                    ::serde_schema::expr::VariantKind::Struct(__fields)
                }),
                ::serde_schema::expr::EnumVariant::new("B", ::serde_schema::expr::VariantKind::Unit)
            ],
        }
    };

    check_enum_to_typeexpr(input, expected);
}

#[test]
fn test_enum_tag_attr_rejects_tuple_variant() {
    let input: DeriveInput = syn::parse_str(
        r#"
        #[serde(tag = "type")]
        enum E {
            A(u8),
        }
        "#,
    )
    .expect("parse input");

    let err = expand_serde_schema(&input).expect_err("expected error");
    let msg = err.to_string();
    assert!(msg.contains("tag"), "msg was: {msg}");
}

#[test]
fn test_enum_external_repr() {
    let input = r#"
        enum E {
            Unit,
            New(u8),
            Tup(u8, i32),
            Struct { a: u8 },
        }
    "#;

    let expected = quote! {
        ::serde_schema::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::None,
            variants: vec![
                ::serde_schema::expr::EnumVariant::new("Unit", ::serde_schema::expr::VariantKind::Unit),
                ::serde_schema::expr::EnumVariant::new(
                    "New",
                    ::serde_schema::expr::VariantKind::Newtype(::std::boxed::Box::new(
                        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
                    ))
                ),
                ::serde_schema::expr::EnumVariant::new(
                    "Tup",
                    ::serde_schema::expr::VariantKind::Tuple(vec![
                        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8),
                        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::I32)
                    ])
                ),
                ::serde_schema::expr::EnumVariant::new("Struct", {
                    let mut __fields: ::std::vec::Vec<::serde_schema::expr::Field> = ::std::vec::Vec::new();
                    __fields.push(::serde_schema::expr::Field::new(
                        "a",
                        ::serde_schema::expr::TypeExpr::Primitive(::serde_schema::expr::PrimitiveType::U8)
                    ));
                    ::serde_schema::expr::VariantKind::Struct(__fields)
                })
            ],
        }
    };

    check_enum_to_typeexpr(input, expected);
}
