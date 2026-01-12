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
    let on_register_body = build_on_register_body(input, &container, sc)?;

    Ok(quote! {
        impl #impl_generics #sc::SerdeSchema for #ident #ty_generics #where_clause {
            fn build_type_expr(ctxt: &mut #sc::context::Context) -> #sc::expr::TypeExpr {
                #body_expr
            }

            fn on_register(registry: &mut #sc::registry::RegistryContext) {
                #on_register_body
            }
        }
    })
}

fn build_on_register_body(
    input: &DeriveInput,
    container: &ContainerSerdeAttrs,
    sc: &syn::Path,
) -> syn::Result<TokenStream2> {
    let mut deps: Vec<Type> = Vec::new();
    collect_deps_from_input(input, container, &mut deps)?;

    // De-dup by token string (stable enough for our purposes).
    let mut uniq: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut dep_calls: Vec<TokenStream2> = Vec::new();
    for ty in deps {
        let key = ty.to_token_stream().to_string();
        if !uniq.insert(key) {
            continue;
        }
        dep_calls.push(quote! {
            <#ty as #sc::SerdeSchema>::on_register(registry);
        });
    }

    Ok(quote! {
        if registry.is_pending::<Self>() {
            return;
        }
        registry.set_pending::<Self>(true);
        #(#dep_calls)*
        registry.try_insert_with(::std::any::TypeId::of::<Self>(), Self::type_expr);
        registry.set_pending::<Self>(false);
    })
}

fn collect_deps_from_input(
    input: &DeriveInput,
    container: &ContainerSerdeAttrs,
    out: &mut Vec<Type>,
) -> syn::Result<()> {
    match &input.data {
        Data::Struct(ds) => collect_deps_from_struct(ds, container, out),
        Data::Enum(de) => collect_deps_from_enum(de, container, out),
        Data::Union(_u) => Ok(()),
    }
}

fn collect_deps_from_struct(
    ds: &DataStruct,
    container: &ContainerSerdeAttrs,
    out: &mut Vec<Type>,
) -> syn::Result<()> {
    match &ds.fields {
        Fields::Unit => Ok(()),
        Fields::Unnamed(fields) => {
            if container.transparent && fields.unnamed.len() == 1 {
                collect_custom_types_from_type(&fields.unnamed[0].ty, true, out);
                return Ok(());
            }
            for f in &fields.unnamed {
                collect_custom_types_from_type(&f.ty, true, out);
            }
            Ok(())
        }
        Fields::Named(fields) => {
            if container.transparent {
                // Same rule as build_type_expr: exactly one non-skipped field.
                let mut non_skipped: Vec<&syn::Field> = Vec::new();
                for f in &fields.named {
                    let f_attrs = parse_field_serde_attrs(&f.attrs)?;
                    if !f_attrs.skip_serializing {
                        non_skipped.push(f);
                    }
                }
                if non_skipped.len() == 1 {
                    collect_custom_types_from_type(&non_skipped[0].ty, true, out);
                    return Ok(());
                }
                // If it's invalid, build_type_expr already errors; keep on_register permissive.
            }

            for f in &fields.named {
                let f_attrs = parse_field_serde_attrs(&f.attrs)?;
                if f_attrs.skip_serializing {
                    continue;
                }
                if f_attrs.flatten {
                    // Flatten delegates to the underlying type's SerdeSchema (e.g. Box<T> -> T),
                    // so register that "flatten target" type.
                    collect_flatten_target_type(&f.ty, out);
                    continue;
                }
                collect_custom_types_from_type(&f.ty, true, out);
            }
            Ok(())
        }
    }
}

fn collect_deps_from_enum(
    de: &DataEnum,
    container: &ContainerSerdeAttrs,
    out: &mut Vec<Type>,
) -> syn::Result<()> {
    // Container-level flags don't affect dependency discovery (only what fields are present).
    let _ = container;

    for v in &de.variants {
        let v_attrs = parse_variant_serde_attrs(&v.attrs)?;
        if v_attrs.skip_serializing {
            continue;
        }

        match &v.fields {
            Fields::Unit => {}
            Fields::Unnamed(fields) => {
                for f in &fields.unnamed {
                    collect_custom_types_from_type(&f.ty, true, out);
                }
            }
            Fields::Named(fields) => {
                for f in &fields.named {
                    let f_attrs = parse_field_serde_attrs(&f.attrs)?;
                    if f_attrs.skip_serializing {
                        continue;
                    }
                    if f_attrs.flatten {
                        collect_flatten_target_type(&f.ty, out);
                        continue;
                    }
                    collect_custom_types_from_type(&f.ty, true, out);
                }
            }
        }
    }
    Ok(())
}

fn collect_custom_types_from_type(ty: &Type, allow_remote: bool, out: &mut Vec<Type>) {
    // Keep this in sync with `type_to_typeexpr`'s type-shape handling.
    match ty {
        Type::Reference(r) => return collect_custom_types_from_type(&r.elem, allow_remote, out),
        Type::Group(g) => return collect_custom_types_from_type(&g.elem, allow_remote, out),
        Type::Paren(p) => return collect_custom_types_from_type(&p.elem, allow_remote, out),
        _ => {}
    }

    if let Type::Tuple(t) = ty {
        for e in &t.elems {
            collect_custom_types_from_type(e, true, out);
        }
        return;
    }

    if let Type::Slice(s) = ty {
        collect_custom_types_from_type(&s.elem, true, out);
        return;
    }

    if let Type::Array(a) = ty {
        collect_custom_types_from_type(&a.elem, true, out);
        return;
    }

    if let Type::Path(p) = ty {
        let last = p
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_default();

        if primitive_from_ident(&last).is_some() {
            return;
        }
        if last == "String" || last == "str" {
            return;
        }
        if last == "Box" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                collect_custom_types_from_type(inner, allow_remote, out);
            }
            return;
        }
        if last == "Option" || last == "Vec" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                collect_custom_types_from_type(inner, true, out);
            }
            return;
        }
        if last == "HashMap" || last == "BTreeMap" {
            if let Some((k, v)) = two_type_args(p.path.segments.last().unwrap()) {
                collect_custom_types_from_type(k, true, out);
                collect_custom_types_from_type(v, true, out);
            }
            return;
        }

        if allow_remote {
            out.push(ty.clone());
        }
        return;
    }
}

fn collect_flatten_target_type(ty: &Type, out: &mut Vec<Type>) {
    // Mirror `type_to_typeexpr(..., allow_remote = false)` behavior for flatten:
    // strip refs/groups/parens and unwrap Box<T>.
    let mut cur = ty;
    loop {
        match cur {
            Type::Reference(r) => {
                cur = &r.elem;
                continue;
            }
            Type::Group(g) => {
                cur = &g.elem;
                continue;
            }
            Type::Paren(p) => {
                cur = &p.elem;
                continue;
            }
            Type::Path(p) => {
                let last = p.path.segments.last().map(|s| s.ident.to_string());
                if last.as_deref() == Some("Box") {
                    if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                        cur = inner;
                        continue;
                    }
                }
            }
            _ => {}
        }
        break;
    }

    if matches!(cur, Type::Path(_)) {
        out.push(cur.clone());
    }
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
                return Ok(type_to_typeexpr(inner_ty, sc, true));
            }

            if fields.unnamed.len() == 1 {
                let inner = type_to_typeexpr(&fields.unnamed[0].ty, sc, true);
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
                .map(|f| type_to_typeexpr(&f.ty, sc, true))
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

                return Ok(type_to_typeexpr(&non_skipped[0].ty, sc, true));
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
        // `#[serde(tag = "...")]` (internally tagged).
        //
        // supports unit/struct/newtype variants for internally tagged enums.
        for v in &de.variants {
            match &v.fields {
                Fields::Unit | Fields::Named(_) => {}
                Fields::Unnamed(fields) => {
                    if fields.unnamed.len() == 1 {
                        continue;
                    }
                    return Err(Error::new(
                        v.span(),
                        "serde_sc: #[serde(tag = ...)] is only supported for enums with unit/struct/newtype variants",
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
                    let inner = type_to_typeexpr(&fields.unnamed[0].ty, sc, true);
                    quote! { #sc::expr::VariantKind::Newtype(::std::boxed::Box::new(#inner)) }
                } else {
                    let elems = fields
                        .unnamed
                        .iter()
                        .map(|f| type_to_typeexpr(&f.ty, sc, true))
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
            let inner = type_to_typeexpr(&f.ty, sc, false);
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

        let ty_expr = type_to_typeexpr(&f.ty, sc, true);
        field_stmts.push(quote! {
            __fields.push(#sc::expr::Field::new(#ser_name, #ty_expr));
        });
    }

    Ok(field_stmts)
}

fn type_to_typeexpr(ty: &Type, sc: &syn::Path, allow_remote: bool) -> TokenStream2 {
    // Strip references / groups / parens.
    match ty {
        Type::Reference(r) => return type_to_typeexpr(&r.elem, sc, allow_remote),
        Type::Group(g) => return type_to_typeexpr(&g.elem, sc, allow_remote),
        Type::Paren(p) => return type_to_typeexpr(&p.elem, sc, allow_remote),
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
            .map(|t| type_to_typeexpr(t, sc, true))
            .collect::<Vec<_>>();
        return quote! { #sc::expr::TypeExpr::Tuple { elements: vec![#(#elems),*] } };
    }

    // Arrays / slices.
    if let Type::Slice(s) = ty {
        if is_u8(&s.elem) {
            return quote! { #sc::expr::TypeExpr::Bytes };
        }
        let elem = type_to_typeexpr(&s.elem, sc, true);
        return quote! { #sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(#elem) } };
    }

    if let Type::Array(a) = ty {
        if is_u8(&a.elem) {
            return quote! { #sc::expr::TypeExpr::Bytes };
        }
        let elem = type_to_typeexpr(&a.elem, sc, true);
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
                return type_to_typeexpr(inner, sc, allow_remote);
            }
        }

        // Option<T>
        if last == "Option" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                let inner = type_to_typeexpr(inner, sc, true);
                return quote! { #sc::expr::TypeExpr::Option(::std::boxed::Box::new(#inner)) };
            }
        }

        // Vec<T>
        if last == "Vec" {
            if let Some(inner) = first_type_arg(p.path.segments.last().unwrap()) {
                if is_u8(inner) {
                    return quote! { #sc::expr::TypeExpr::Bytes };
                }
                let inner = type_to_typeexpr(inner, sc, true);
                return quote! { #sc::expr::TypeExpr::Seq { element: ::std::boxed::Box::new(#inner) } };
            }
        }

        // HashMap<K,V> / BTreeMap<K,V>
        if last == "HashMap" || last == "BTreeMap" {
            if let Some((k, v)) = two_type_args(p.path.segments.last().unwrap()) {
                let k = type_to_typeexpr(k, sc, true);
                let v = type_to_typeexpr(v, sc, true);
                return quote! {
                    #sc::expr::TypeExpr::Map {
                        key: ::std::boxed::Box::new(#k),
                        value: ::std::boxed::Box::new(#v),
                    }
                };
            }
        }

        if allow_remote {
            // Fallback: use TypeExpr::Remote to refer to non-special-cased types by TypeId.
            return quote! { #sc::expr::TypeExpr::Remote {
                path: ::std::borrow::Cow::Borrowed(stringify!(#p)),
                type_id: ::std::any::TypeId::of::<#ty>()
            } };
        } else {
            // Fallback: delegate to `SerdeSchema` of the referenced type.
            return quote! { <#ty as #sc::SerdeSchema>::build_type_expr_no_recursion(ctxt) };
        }
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
        "isize" => "I64",
        "u8" => "U8",
        "u16" => "U16",
        "u32" => "U32",
        "u64" => "U64",
        "u128" => "U128",
        "usize" => "U64",
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
