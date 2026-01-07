use convert_case::Case;
use syn::{Error, LitStr, spanned::Spanned};

#[derive(Debug, Default, Clone)]
pub(crate) struct ContainerSerdeAttrs {
    pub(crate) rename: Option<String>,
    pub(crate) rename_all: Option<Case<'static>>,
    pub(crate) transparent: bool,
    pub(crate) untagged: bool,
    pub(crate) tag: Option<String>,
    pub(crate) content: Option<String>,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct VariantSerdeAttrs {
    pub(crate) rename: Option<String>,
    pub(crate) rename_all: Option<Case<'static>>,
    pub(crate) skip_serializing: bool,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct FieldSerdeAttrs {
    pub(crate) rename: Option<String>,
    pub(crate) skip_serializing: bool,
    pub(crate) flatten: bool,
}

fn parse_serde_rename_all(s: &str) -> Option<Case<'static>> {
    Some(match s {
        "lowercase" => Case::Lower,
        "UPPERCASE" => Case::Upper,
        "snake_case" => Case::Snake,
        "kebab-case" => Case::Kebab,
        "camelCase" => Case::Camel,
        "PascalCase" => Case::Pascal,
        "SCREAMING_SNAKE_CASE" => Case::UpperSnake,
        "SCREAMING-KEBAB-CASE" => Case::UpperKebab,
        _ => return None,
    })
}

pub(crate) fn parse_container_serde_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<ContainerSerdeAttrs> {
    let mut out = ContainerSerdeAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("transparent") {
                out.transparent = true;
                return Ok(());
            }
            if meta.path.is_ident("untagged") {
                out.untagged = true;
                return Ok(());
            }
            if meta.path.is_ident("tag") {
                let v: LitStr = meta.value()?.parse()?;
                out.tag = Some(v.value());
                return Ok(());
            }
            if meta.path.is_ident("content") {
                let v: LitStr = meta.value()?.parse()?;
                out.content = Some(v.value());
                return Ok(());
            }
            if meta.path.is_ident("rename") {
                let v: LitStr = meta.value()?.parse()?;
                out.rename = Some(v.value());
                return Ok(());
            }
            if meta.path.is_ident("rename_all") {
                let v: LitStr = meta.value()?.parse()?;
                out.rename_all = Some(parse_serde_rename_all(&v.value()).ok_or_else(|| {
                    Error::new(
                        v.span(),
                        format!("unsupported rename_all rule: {}", v.value()),
                    )
                })?);
                return Ok(());
            }
            Ok(())
        })?;
    }

    Ok(out)
}

pub(crate) fn parse_variant_serde_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<VariantSerdeAttrs> {
    let mut out = VariantSerdeAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("untagged") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(untagged)] is not supported",
                ));
            }
            if meta.path.is_ident("tag") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(tag = ...)] is only supported on enum containers",
                ));
            }
            if meta.path.is_ident("content") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(content = ...)] is only supported on enum containers",
                ));
            }
            if meta.path.is_ident("skip") || meta.path.is_ident("skip_serializing") {
                out.skip_serializing = true;
                return Ok(());
            }
            if meta.path.is_ident("rename") {
                let v: LitStr = meta.value()?.parse()?;
                out.rename = Some(v.value());
                return Ok(());
            }
            if meta.path.is_ident("rename_all") {
                let v: LitStr = meta.value()?.parse()?;
                out.rename_all = Some(parse_serde_rename_all(&v.value()).ok_or_else(|| {
                    Error::new(
                        v.span(),
                        format!("unsupported rename_all rule: {}", v.value()),
                    )
                })?);
                return Ok(());
            }
            Ok(())
        })?;
    }

    Ok(out)
}

pub(crate) fn parse_field_serde_attrs(attrs: &[syn::Attribute]) -> syn::Result<FieldSerdeAttrs> {
    let mut out = FieldSerdeAttrs::default();

    for attr in attrs {
        if !attr.path().is_ident("serde") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("untagged") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(untagged)] is not supported",
                ));
            }
            if meta.path.is_ident("tag") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(tag = ...)] is only supported on enum containers",
                ));
            }
            if meta.path.is_ident("content") {
                return Err(Error::new(
                    meta.path.span(),
                    "serde_schema: #[serde(content = ...)] is only supported on enum containers",
                ));
            }
            if meta.path.is_ident("skip") || meta.path.is_ident("skip_serializing") {
                out.skip_serializing = true;
                return Ok(());
            }
            if meta.path.is_ident("flatten") {
                out.flatten = true;
                return Ok(());
            }
            if meta.path.is_ident("rename") {
                let v: LitStr = meta.value()?.parse()?;
                out.rename = Some(v.value());
                return Ok(());
            }
            Ok(())
        })?;
    }

    Ok(out)
}
