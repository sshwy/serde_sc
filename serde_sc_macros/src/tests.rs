use quote::quote;
use syn::{Data, DeriveInput};

use crate::{
    enum_to_typeexpr, expand_serde_schema, parse_container_serde_attrs,
    parse_container_serde_sc_attrs, struct_to_typeexpr,
};

fn check_struct_to_typeexpr(input: &str, expected: proc_macro2::TokenStream) {
    let input: DeriveInput = syn::parse_str(input).expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");
    let sc = parse_container_serde_sc_attrs(&input.attrs).expect("parse serde_sc attrs");
    let sc = &sc.crate_path;

    let ds = match &input.data {
        Data::Struct(ds) => ds,
        _ => panic!("expected struct"),
    };

    let got = struct_to_typeexpr(&input.ident, ds, &container, sc).expect("struct_to_typeexpr");
    assert_eq!(got.to_string(), expected.to_string());
}

fn check_enum_to_typeexpr(input: &str, expected: proc_macro2::TokenStream) {
    let input: DeriveInput = syn::parse_str(input).expect("parse input");
    let container = parse_container_serde_attrs(&input.attrs).expect("parse container attrs");
    let sc = parse_container_serde_sc_attrs(&input.attrs).expect("parse serde_sc attrs");
    let sc = &sc.crate_path;

    let de = match &input.data {
        Data::Enum(de) => de,
        _ => panic!("expected enum"),
    };

    let got = enum_to_typeexpr(&input.ident, de, &container, sc).expect("enum_to_typeexpr");
    assert_eq!(got.to_string(), expected.to_string());
}

#[test]
fn test_unit_struct() {
    let expected = quote! {
        ::serde_sc::expr::TypeExpr::UnitStruct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new("a", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::Bool)));
        __fields.push(::serde_sc::expr::Field::new("b", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I8)));
        __fields.push(::serde_sc::expr::Field::new("c", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I16)));
        __fields.push(::serde_sc::expr::Field::new("d", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)));
        __fields.push(::serde_sc::expr::Field::new("e", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I64)));
        __fields.push(::serde_sc::expr::Field::new("f", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I128)));
        __fields.push(::serde_sc::expr::Field::new("g", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)));
        __fields.push(::serde_sc::expr::Field::new("h", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U16)));
        __fields.push(::serde_sc::expr::Field::new("i", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)));
        __fields.push(::serde_sc::expr::Field::new("j", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U64)));
        __fields.push(::serde_sc::expr::Field::new("k", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U128)));
        __fields.push(::serde_sc::expr::Field::new("l", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::F32)));
        __fields.push(::serde_sc::expr::Field::new("m", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::F64)));
        __fields.push(::serde_sc::expr::Field::new("n", ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::Char)));
        ::serde_sc::expr::TypeExpr::Struct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new("a", ::serde_sc::expr::TypeExpr::String));
        __fields.push(::serde_sc::expr::Field::new("b", ::serde_sc::expr::TypeExpr::String));
        ::serde_sc::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_serde_default_path_is_accepted_on_field() {
    let input = r#"
        struct S {
            #[serde(default = "some::path::to::default_a")]
            a: u32,
        }
        "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new(
            "a",
            ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)
        ));
        ::serde_sc::expr::TypeExpr::Struct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new("a", ::serde_sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)) }));
        __fields.push(::serde_sc::expr::Field::new("b", ::serde_sc::expr::TypeExpr::Bytes));
        __fields.push(::serde_sc::expr::Field::new("c", ::serde_sc::expr::TypeExpr::Bytes));
        __fields.push(::serde_sc::expr::Field::new("d", ::serde_sc::expr::TypeExpr::Tuple { elements: vec![::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32); 3usize] }));
        __fields.push(::serde_sc::expr::Field::new("e", ::serde_sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)) }));
        __fields.push(::serde_sc::expr::Field::new("f", ::serde_sc::expr::TypeExpr::Bytes));
        ::serde_sc::expr::TypeExpr::Struct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new("a", ::serde_sc::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::String), value: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)), }));
        __fields.push(::serde_sc::expr::Field::new("b", ::serde_sc::expr::TypeExpr::Map { key: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)), value: ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Bytes), }));
        ::serde_sc::expr::TypeExpr::Struct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new("a", ::serde_sc::expr::TypeExpr::Unit));
        ::serde_sc::expr::TypeExpr::Struct {
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new(
            "a",
            ::serde_sc::expr::TypeExpr::Option(
                ::std::boxed::Box::new(
                    ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)
                )
            )
        ));
        __fields.push(::serde_sc::expr::Field::new(
            "b",
            ::serde_sc::expr::TypeExpr::Option(
                ::std::boxed::Box::new(::serde_sc::expr::TypeExpr::Bytes)
            )
        ));
        ::serde_sc::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_newtype_struct() {
    let expected = quote! {
        ::serde_sc::expr::TypeExpr::NewtypeStruct {
            name: ::std::borrow::Cow::Borrowed("New"),
            inner: ::std::boxed::Box::new(
                ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
            ),
        }
    };
    check_struct_to_typeexpr("struct New(u8);", expected);
}

#[test]
fn test_tuple_struct() {
    let expected = quote! {
        ::serde_sc::expr::TypeExpr::TupleStruct {
            name: ::std::borrow::Cow::Borrowed("Tup"),
            elements: vec![
                ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8),
                ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new(
            "a",
            ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
        ));
        ::serde_sc::expr::TypeExpr::Struct {
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
        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)
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
        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U32)
    };
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_struct_remote_fallback() {
    let input = r#"
        struct Outer {
            inner: Inner,
            a: u8,
        }
    "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(::serde_sc::expr::Field::new(
            "inner",
            ::serde_sc::remote_type!(Inner)
        ));
        __fields.push(::serde_sc::expr::Field::new(
            "a",
            ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
        ));
        ::serde_sc::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("Outer"),
            fields: __fields,
        }
    }};
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
        let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
        {
            let __inner = <Inner as ::serde_sc::SerdeSchema>::build_type_expr_no_recursion(ctxt);
            match __inner {
                ::serde_sc::expr::TypeExpr::Struct { name: _, fields } => {
                    __fields.extend(fields);
                }
                _other => {
                    panic!(
                        "serde_sc: #[serde(flatten)] is only supported for fields whose SerdeSchema is TypeExpr::Struct"
                    );
                }
            }
        }
        __fields.push(::serde_sc::expr::Field::new(
            "a",
            ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
        ));
        ::serde_sc::expr::TypeExpr::Struct {
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
        ::serde_sc::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::Some(::std::borrow::Cow::Borrowed("type")),
            content: ::std::option::Option::None,
            variants: vec![
                ::serde_sc::expr::EnumVariant::new("A", {
                    let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
                    __fields.push(::serde_sc::expr::Field::new(
                        "a",
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ));
                    ::serde_sc::expr::VariantKind::Struct(__fields)
                }),
                ::serde_sc::expr::EnumVariant::new("B", ::serde_sc::expr::VariantKind::Unit)
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
            A(u8, i32),
        }
        "#,
    )
    .expect("parse input");

    let err = expand_serde_schema(&input).expect_err("expected error");
    let msg = err.to_string();
    assert!(msg.contains("tag"), "msg was: {msg}");
}

#[test]
fn test_enum_tag_attr_allows_newtype_variant() {
    let input = r#"
        #[serde(tag = "type")]
        enum E {
            A(u8),
        }
    "#;

    let expected = quote! {
        ::serde_sc::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::Some(::std::borrow::Cow::Borrowed("type")),
            content: ::std::option::Option::None,
            variants: vec![
                ::serde_sc::expr::EnumVariant::new(
                    "A",
                    ::serde_sc::expr::VariantKind::Newtype(::std::boxed::Box::new(
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ))
                )
            ],
        }
    };

    check_enum_to_typeexpr(input, expected);
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
        ::serde_sc::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::None,
            content: ::std::option::Option::None,
            variants: vec![
                ::serde_sc::expr::EnumVariant::new("Unit", ::serde_sc::expr::VariantKind::Unit),
                ::serde_sc::expr::EnumVariant::new(
                    "New",
                    ::serde_sc::expr::VariantKind::Newtype(::std::boxed::Box::new(
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ))
                ),
                ::serde_sc::expr::EnumVariant::new(
                    "Tup",
                    ::serde_sc::expr::VariantKind::Tuple(vec![
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8),
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)
                    ])
                ),
                ::serde_sc::expr::EnumVariant::new("Struct", {
                    let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
                    __fields.push(::serde_sc::expr::Field::new(
                        "a",
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ));
                    ::serde_sc::expr::VariantKind::Struct(__fields)
                })
            ],
        }
    };

    check_enum_to_typeexpr(input, expected);
}

#[test]
fn test_enum_adjacent_tag_attr() {
    let input = r#"
        #[serde(tag = "t", content = "c")]
        enum E {
            Unit,
            New(u8),
            Tup(u8, i32),
            Struct { a: u8 },
        }
    "#;

    let expected = quote! {
        ::serde_sc::expr::TypeExpr::Enum {
            name: ::std::borrow::Cow::Borrowed("E"),
            tag: ::std::option::Option::Some(::std::borrow::Cow::Borrowed("t")),
            content: ::std::option::Option::Some(::std::borrow::Cow::Borrowed("c")),
            variants: vec![
                ::serde_sc::expr::EnumVariant::new("Unit", ::serde_sc::expr::VariantKind::Unit),
                ::serde_sc::expr::EnumVariant::new(
                    "New",
                    ::serde_sc::expr::VariantKind::Newtype(::std::boxed::Box::new(
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ))
                ),
                ::serde_sc::expr::EnumVariant::new(
                    "Tup",
                    ::serde_sc::expr::VariantKind::Tuple(vec![
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8),
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::I32)
                    ])
                ),
                ::serde_sc::expr::EnumVariant::new("Struct", {
                    let mut __fields: ::std::vec::Vec<::serde_sc::expr::Field> = ::std::vec::Vec::new();
                    __fields.push(::serde_sc::expr::Field::new(
                        "a",
                        ::serde_sc::expr::TypeExpr::Primitive(::serde_sc::expr::PrimitiveType::U8)
                    ));
                    ::serde_sc::expr::VariantKind::Struct(__fields)
                })
            ],
        }
    };

    check_enum_to_typeexpr(input, expected);
}

#[test]
fn test_custom_serde_sc_crate_path() {
    let input = r#"
        #[serde_sc(crate = "my::serde_sc")]
        struct S {
            a: u8,
        }
    "#;
    let expected = quote! {{
        let mut __fields: ::std::vec::Vec<my::serde_sc::expr::Field> = ::std::vec::Vec::new();
        __fields.push(my::serde_sc::expr::Field::new(
            "a",
            my::serde_sc::expr::TypeExpr::Primitive(my::serde_sc::expr::PrimitiveType::U8)
        ));
        my::serde_sc::expr::TypeExpr::Struct {
            name: ::std::borrow::Cow::Borrowed("S"),
            fields: __fields,
        }
    }};
    check_struct_to_typeexpr(input, expected);
}

#[test]
fn test_container_serde_attr() {
    let input: DeriveInput = syn::parse_str(
        r#"
        #[serde(bound = "T: ::serde::Serialize")]
        struct S<T> {
            a: T,
        }
        "#,
    )
    .expect("parse input");

    // Should accept container-level `#[serde(...)]` attributes (even those serde_sc doesn't use).
    let _ts = expand_serde_schema(&input).expect("expand_serde_schema");
}

#[test]
fn test_variant_serde_attr() {
    let input: DeriveInput = syn::parse_str(
        r#"
        enum E {
            #[serde(alias = "A1")]
            A,
        }
        "#,
    )
    .expect("parse input");

    // Should accept variant-level `#[serde(...)]` attributes (even those serde_sc doesn't use).
    let _ts = expand_serde_schema(&input).expect("expand_serde_schema");
}

#[test]
fn test_field_serde_attr() {
    let input: DeriveInput = syn::parse_str(
        r#"
        struct S {
            #[serde(with = "some::path")]
            a: u8,
        }
        "#,
    )
    .expect("parse input");

    // Should accept field-level `#[serde(...)]` attributes (even those serde_sc doesn't use).
    let _ts = expand_serde_schema(&input).expect("expand_serde_schema");
}

#[test]
fn test_serde_as_attr() {
    let input: DeriveInput = syn::parse_str(
        r#"
        #[serde_as]
        #[derive(Debug, Deserialize, SerdeSchema, Clone)]
        pub struct CursorQuery {
            #[serde_as(as = "DisplayFromStr")]
            max_count: u8,
            #[serde_as(as = "DisplayFromStr")]
            start_id: u32,
        }
        "#,
    )
    .expect("parse input");

    // Should accept container-level `#[serde(...)]` attributes (even those serde_sc doesn't use).
    let _ts = expand_serde_schema(&input).expect("expand_serde_schema");
}
