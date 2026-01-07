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
