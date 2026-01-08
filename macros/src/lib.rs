//! Proc-macro crate for `serde_sc`.

extern crate proc_macro;
mod attrs;
#[cfg(test)]
mod tests;

use proc_macro::TokenStream;

use convert_case::{Case, Casing};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{ToTokens, quote};
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Error, Fields, GenericParam, LitStr, PathArguments,
    Type, parse_macro_input, spanned::Spanned,
};

use crate::attrs::{
    ContainerSerdeAttrs, parse_container_serde_attrs, parse_container_serde_sc_attrs,
    parse_field_serde_attrs, parse_variant_serde_attrs,
};

/// Derive macro that generates an implementation of `serde_sc::SerdeSchema`.
///
/// It inspects struct/enum fields and a subset of Serde attributes
/// (see <https://serde.rs/attributes.html>), then produces `TypeExpr` construction code.
#[proc_macro_derive(SerdeSchema, attributes(serde, serde_sc))]
pub fn derive_serde_schema(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match expand_serde_schema(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

/// Parse the input type and generate a `TypeExpr` expression.
fn expand_serde_schema(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut where_clause = where_clause.cloned();
    let sc = parse_container_serde_sc_attrs(&input.attrs)?;
    let sc = &sc.crate_path;
    add_serde_schema_bounds(&mut where_clause, &input.generics, sc);

    let container = parse_container_serde_attrs(&input.attrs)?;

    let body_expr = type_def_to_typeexpr(input, &container, sc)?;

    Ok(quote! {
        impl #impl_generics #sc::SerdeSchema for #ident #ty_generics #where_clause {
            fn serde_sc() -> #sc::expr::TypeExpr {
                #body_expr
            }
        }
    })
}

fn add_serde_schema_bounds(
    where_clause: &mut Option<syn::WhereClause>,
    generics: &syn::Generics,
    sc: &syn::Path,
) {
    let wc = where_clause.get_or_insert_with(|| syn::WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });

    for param in &generics.params {
        let GenericParam::Type(ty) = param else {
            continue;
        };
        let ident = &ty.ident;
        wc.predicates
            .push(syn::parse_quote!(#ident: #sc::SerdeSchema));
    }
}

fn type_def_to_typeexpr(
    input: &DeriveInput,
    container: &ContainerSerdeAttrs,
    sc: &syn::Path,
) -> syn::Result<TokenStream2> {
    match &input.data {
        Data::Struct(ds) => struct_to_typeexpr(&input.ident, ds, container, sc),
        Data::Enum(de) => enum_to_typeexpr(&input.ident, de, container, sc),
        Data::Union(u) => Err(Error::new(
            u.union_token.span(),
            "SerdeSchema does not support unions",
        )),
    }
}

fn struct_to_typeexpr(
    ident: &syn::Ident,
    ds: &DataStruct,
    container: &ContainerSerdeAttrs,
    sc: &syn::Path,
) -> syn::Result<TokenStream2> {
    if container.tag.is_some() || container.content.is_some() {
        return Err(Error::new(
            ident.span(),
            "serde_sc: #[serde(tag/content = ...)] is only supported on enum containers",
        ));
    }

    // Container rename affects the "type name" we store.
    let name = container
        .rename
        .clone()
        .unwrap_or_else(|| ident.to_string());
    let name_ts = quote_identexpr(&name);

    match &ds.fields {
        Fields::Unit => Ok(quote! {
            #sc::expr::TypeExpr::UnitStruct { name: #name_ts }
        }),
        Fields::Unnamed(fields) => {
            if container.transparent && fields.unnamed.len() != 1 {
                return Err(Error::new(
                    fields.span(),
                    "#[serde(transparent)] requires exactly one field",
                ));
            }
            if fields.unnamed.len() == 1 && container.transparent {
                // `#[serde(transparent)]` newtype struct: serialize as inner.
                let inner_ty = &fields.unnamed[0].ty;
                return Ok(type_to_typeexpr(inner_ty, sc));
            }

            if fields.unnamed.len() == 1 {
                let inner = type_to_typeexpr(&fields.unnamed[0].ty, sc);
                return Ok(quote! {
                    #sc::expr::TypeExpr::NewtypeStruct {
                        name: #name_ts,
                        inner: ::std::boxed::Box::new(#inner),
                    }
                });
            }

            let elems = fields
                .unnamed
                .iter()
                .map(|f| type_to_typeexpr(&f.ty, sc))
                .collect::<Vec<_>>();
            Ok(quote! {
                #sc::expr::TypeExpr::TupleStruct {
                    name: #name_ts,
                    elements: vec![#(#elems),*],
                }
            })
        }
        Fields::Named(fields) => {
            if container.transparent {
                let mut non_skipped = Vec::new();
                for f in &fields.named {
                    let f_attrs = parse_field_serde_attrs(&f.attrs)?;
                    if !f_attrs.skip_serializing {
                        non_skipped.push(f);
                    }
                }

                if non_skipped.len() != 1 {
                    return Err(Error::new(
                        fields.span(),
                        "#[serde(transparent)] requires exactly one field",
                    ));
                }

                return Ok(type_to_typeexpr(&non_skipped[0].ty, sc));
            }

            let field_stmts = named_fields_to_field_stmts(fields, container.rename_all, sc)?;

            Ok(quote! {{
                let mut __fields: ::std::vec::Vec<#sc::expr::Field> = ::std::vec::Vec::new();
                #(#field_stmts)*
                #sc::expr::TypeExpr::Struct {
                    name: #name_ts,
                    fields: __fields,
                }
            }})
        }
    }
}

fn enum_to_typeexpr(
    ident: &syn::Ident,
    de: &DataEnum,
    container: &ContainerSerdeAttrs,
    sc: &syn::Path,
) -> syn::Result<TokenStream2> {
    if container.untagged {
        return Err(Error::new(
            ident.span(),
            "serde_sc: #[serde(untagged)] is not supported",
        ));
    }

    if container.content.is_some() && container.tag.is_none() {
        return Err(Error::new(
            ident.span(),
            "serde_sc: #[serde(content = ...)] requires #[serde(tag = ...)]",
        ));
    }

    if container.tag.is_some() && container.content.is_none() {
        // `#[serde(tag = "...")]` (internally tagged) only supports unit and struct variants.
        for v in &de.variants {
            match &v.fields {
                Fields::Unit | Fields::Named(_) => {}
                Fields::Unnamed(_) => {
                    return Err(Error::new(
                        v.span(),
                        "serde_sc: #[serde(tag = ...)] is only supported for enums with unit/struct variants",
                    ));
                }
            }
        }
    }

    let name = container
        .rename
        .clone()
        .unwrap_or_else(|| ident.to_string());
    let name_ts = quote_identexpr(&name);
    let tag_ts = container
        .tag
        .as_deref()
        .map(|t| quote! { ::std::option::Option::Some(::std::borrow::Cow::Borrowed(#t)) })
        .unwrap_or_else(|| quote! { ::std::option::Option::None });
    let content_ts = container
        .content
        .as_deref()
        .map(|c| quote! { ::std::option::Option::Some(::std::borrow::Cow::Borrowed(#c)) })
        .unwrap_or_else(|| quote! { ::std::option::Option::None });

    let mut variants_ts = Vec::new();
    for v in &de.variants {
        let v_attrs = parse_variant_serde_attrs(&v.attrs)?;
        if v_attrs.skip_serializing {
            continue;
        }

        let original_vname = v.ident.to_string();
        let has_rename = v_attrs.rename.is_some();
        let mut ser_vname = v_attrs.rename.unwrap_or_else(|| original_vname.clone());
        if !has_rename {
            if let Some(rule) = container.rename_all {
                ser_vname = apply_rename_all(rule, &original_vname);
            }
        }

        let kind_ts = match &v.fields {
            Fields::Unit => quote! { #sc::expr::VariantKind::Unit },
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    let inner = type_to_typeexpr(&fields.unnamed[0].ty, sc);
                    quote! { #sc::expr::VariantKind::Newtype(::std::boxed::Box::new(#inner)) }
                } else {
                    let elems = fields
                        .unnamed
                        .iter()
                        .map(|f| type_to_typeexpr(&f.ty, sc))
                        .collect::<Vec<_>>();
                    quote! { #sc::expr::VariantKind::Tuple(vec![#(#elems),*]) }
                }
            }
            Fields::Named(fields) => {
                let field_stmts = named_fields_to_field_stmts(fields, v_attrs.rename_all, sc)?;

                quote! {{
                    let mut __fields: ::std::vec::Vec<#sc::expr::Field> = ::std::vec::Vec::new();
                    #(#field_stmts)*
                    #sc::expr::VariantKind::Struct(__fields)
                }}
            }
        };

        variants_ts.push(quote! {
            #sc::expr::EnumVariant::new(#ser_vname, #kind_ts)
        });
    }

    Ok(quote! {
        #sc::expr::TypeExpr::Enum {
            name: #name_ts,
            tag: #tag_ts,
            content: #content_ts,
            variants: vec![#(#variants_ts),*],
        }
    })
}

fn quote_identexpr(s: &str) -> TokenStream2 {
    // IdentExpr is `Cow<'static, str>`. Since `s` becomes a string literal in
    // the expanded code, we can use a borrowed `'static` str.
    quote! { ::std::borrow::Cow::Borrowed(#s) }
}

fn apply_rename_all(case: Case, s: &str) -> String {
    s.to_case(case)
}

fn named_fields_to_field_stmts(
    fields: &syn::FieldsNamed,
    rename_all: Option<Case<'static>>,
    sc: &syn::Path,
) -> syn::Result<Vec<TokenStream2>> {
    let mut field_stmts = Vec::new();

    for f in &fields.named {
        let f_ident = f
            .ident
            .as_ref()
            .ok_or_else(|| Error::new(f.span(), "missing field ident"))?;

        let f_attrs = parse_field_serde_attrs(&f.attrs)?;
        if f_attrs.skip_serializing {
            if f_attrs.flatten {
                return Err(Error::new(
                    f.span(),
                    "field cannot be both #[serde(flatten)] and skipped",
                ));
            }
            continue;
        }

        if f_attrs.flatten {
            if f_attrs.rename.is_some() {
                return Err(Error::new(
                    f.span(),
                    "field cannot be both #[serde(flatten)] and #[serde(rename = ...)]",
                ));
            }
            let inner = type_to_typeexpr(&f.ty, sc);
            field_stmts.push(quote! {
                {
                    let __inner = #inner;
                    match __inner {
                        #sc::expr::TypeExpr::Struct { name: _, fields } => {
                            __fields.extend(fields);
                        }
                        _other => {
                            panic!(
                                "serde_sc: #[serde(flatten)] is only supported for fields whose SerdeSchema is TypeExpr::Struct"
                            );
                        }
                    }
                }
            });
            continue;
        }

        let original = f_ident.to_string();
        let has_rename = f_attrs.rename.is_some();
        let mut ser_name = f_attrs.rename.unwrap_or_else(|| original.clone());
        if !has_rename {
            if let Some(rule) = rename_all {
                ser_name = apply_rename_all(rule, &original);
            }
        }

        let ty_expr = type_to_typeexpr(&f.ty, sc);
        field_stmts.push(quote! {
            __fields.push(#sc::expr::Field::new(#ser_name, #ty_expr));
        });
    }

    Ok(field_stmts)
}

fn type_to_typeexpr(ty: &Type, sc: &syn::Path) -> TokenStream2 {
    // Strip references / groups / parens.
    match ty {
        Type::Reference(r) => return type_to_typeexpr(&r.elem, sc),
        Type::Group(g) => return type_to_typeexpr(&g.elem, sc),
        Type::Paren(p) => return type_to_typeexpr(&p.elem, sc),
        _ => {}
    }

    // Unit: `()`
    if let Type::Tuple(t) = ty {
        if t.elems.is_empty() {
            return quote! { #sc::expr::TypeExpr::Unit };
        }
        let elems = t
            .elems
            .iter()
            .map(|t| type_to_typeexpr(t, sc))
            .collect::<Vec<_>>();
        return quote! { #sc::expr::TypeExpr::Tuple { elements: vec![#(#elems),*] } };
    }

    // Arrays / slices.
    if let Type::Slice(s) = ty {
        if is_u8(&s.elem) {
            return quote! { #sc::expr::TypeExpr::Bytes };
        }
        let elem = type_to_typeexpr(&s.elem, sc);
        return quote! { #sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(#elem) } };
    }

    if let Type::Array(a) = ty {
        if is_u8(&a.elem) {
            return quote! { #sc::expr::TypeExpr::Bytes };
        }
        let elem = type_to_typeexpr(&a.elem, sc);
        if let syn::Expr::Lit(lit) = &a.len {
            if let syn::Lit::Int(n) = &lit.lit {
                if let Ok(n) = n.base10_parse::<usize>() {
                    return quote! {
                        #sc::expr::TypeExpr::Tuple { elements: vec![#elem; #n] }
                    };
                }
            }
        }
        return quote! { #sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(#elem) } };
    }

    // Path types.
    if let Type::Path(p) = ty {
        let last = p
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();

        // Primitives
        if let Some(pt) = primitive_from_ident(&last) {
            return quote! { #sc::expr::TypeExpr::Primitive(#sc::expr::PrimitiveType::#pt) };
        }

        // `String` / `str`
        if last == "String" || last == "str" {
            return quote! { #sc::expr::TypeExpr::String };
        }

        // Box<T> serializes as T.
        if last == "Box" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                return type_to_typeexpr(inner, sc);
            }
        }

        // Option<T>
        if last == "Option" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                let inner = type_to_typeexpr(inner, sc);
                return quote! { #sc::expr::TypeExpr::Option(::std::boxed::Box::new(#inner)) };
            }
        }

        // Vec<T>
        if last == "Vec" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                if is_u8(inner) {
                    return quote! { #sc::expr::TypeExpr::Bytes };
                }
                let inner = type_to_typeexpr(inner, sc);
                return quote! { #sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(#inner) } };
            }
        }

        // HashMap<K,V> / BTreeMap<K,V>
        if last == "HashMap" || last == "BTreeMap" {
            if let Some((k, v)) = two_type_args(p.path.segments.last().unwrap()) {
                let k = type_to_typeexpr(k, sc);
                let v = type_to_typeexpr(v, sc);
                return quote! {
                    #sc::expr::TypeExpr::Map {
                        key: ::std::boxed::Box::new(#k),
                        value: ::std::boxed::Box::new(#v),
                    }
                };
            }
        }

        // Fallback: delegate to `SerdeSchema` of the referenced type.
        return quote! { <#ty as #sc::SerdeSchema>::serde_sc() };
    }

    // Fallback for unsupported syn::Type forms.
    let msg = format!("unsupported type for SerdeSchema: {}", ty.to_token_stream());
    let lit = LitStr::new(&msg, Span::call_site());
    quote! { compile_error!(#lit) }
}

fn primitive_from_ident(ident: &str) -> Option<syn::Ident> {
    let p = match ident {
        "bool" => "Bool",
        "i8" => "I8",
        "i16" => "I16",
        "i32" => "I32",
        "i64" => "I64",
        "i128" => "I128",
        "u8" => "U8",
        "u16" => "U16",
        "u32" => "U32",
        "u64" => "U64",
        "u128" => "U128",
        "f32" => "F32",
        "f64" => "F64",
        "char" => "Char",
        _ => return None,
    };
    Some(syn::Ident::new(p, Span::call_site()))
}

fn is_u8(ty: &Type) -> bool {
    matches!(ty, Type::Path(p) if p.path.segments.last().is_some_and(|s| s.ident == "u8"))
}

fn first_type_arg(seg: &syn::PathSegment) -> Option<&Type> {
    let PathArguments::AngleBracketed(args) = &seg.arguments else {
        return None;
    };
    for a in &args.args {
        if let syn::GenericArgument::Type(t) = a {
            return Some(t);
        }
    }
    None
}

fn two_type_args(seg: &syn::PathSegment) -> Option<(&Type, &Type)> {
    let PathArguments::AngleBracketed(args) = &seg.arguments else {
        return None;
    };
    let mut it = args.args.iter().filter_map(|a| match a {
        syn::GenericArgument::Type(t) => Some(t),
        _ => None,
    });
    let a = it.next()?;
    let b = it.next()?;
    Some((a, b))
}
