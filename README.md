# serde_sc

`serde_sc` is a small companion crate for describing Rust types as a **schema** (a `TypeExpr` tree) in terms of the [Serde data model](https://serde.rs/data-model.html).

`serde_sc` lets you:

- Derive a `TypeExpr` (a small AST) for your Rust types using `#[derive(SerdeSchema)]`
- Optionally apply a subset of Serde attributes (rename/rename_all/transparent/flatten/tag/content)
- Collect type schemas (including dependencies) into a `Registry` for later lookup / export

## Getting started

Add it to your `Cargo.toml`:

```toml
[dependencies]
serde_sc = { git = "https://github.com/sshwy/serde_sc", branch = "master" }
```

## Usage

### Get a schema as a `TypeExpr`

Call `T::type_expr()` (provided by the `SerdeSchema` trait):

```rust
use serde_sc::SerdeSchema;

#[derive(SerdeSchema)]
struct Person {
    name: String,
    age: u8,
}

fn main() {
    println!("{:?}", Person::type_expr());
}
```

`TypeExpr` is a tree that can represent primitives, strings, bytes, options, sequences, maps, structs, and enums (plus references to other schema types via `Remote`).

### Derive schemas for real structs/enums (with common Serde attributes)

The derive macro supports a subset of Serde attributes to keep the schema aligned with your serialization format:

```rust
use serde_sc::SerdeSchema;

#[derive(SerdeSchema)]
#[serde(rename_all = "camelCase")]
struct UserProfile {
    user_id: u64,
    display_name: String,
}
```

### Register a type (and its dependencies) into a `Registry`

`Registry` maps `TypeId -> TypeExpr` and will recursively register dependent types discovered by the derive macro:

```rust
use serde_sc::{SerdeSchema, registry::Registry};

#[derive(SerdeSchema)]
struct Address {
    street: String,
    city: String,
    zip: String,
}

#[derive(SerdeSchema)]
struct Person {
    name: String,
    addr: Address,
}

fn main() {
    let mut r = Registry::new();
    r.register::<Person>();
    println!("r = {:?}", r);
}
```

## What gets represented as what?

The derive macro maps common Rust types to Serde data model concepts:

- **Primitives**: `bool`, `i8..i128`, `u8..u128`, `f32`, `f64`, `char` → `TypeExpr::Primitive(..)`
- **Strings**: `String` / `str` → `TypeExpr::String`
- **Bytes**: `Vec<u8>`, `[u8]`, `[u8; N]` → `TypeExpr::Bytes`
- **Option**: `Option<T>` → `TypeExpr::Option(T)`
- **Seq**: `Vec<T>`, `[T]`, `[T; N]` (non-`u8`) → `TypeExpr::Seq` (or `TypeExpr::Tuple` when `N` is a literal)
- **Map**: `HashMap<K, V>`, `BTreeMap<K, V>` → `TypeExpr::Map`
- **Box**: `Box<T>` is treated as `T` (schema-wise)
- **Other path types**: represented as `TypeExpr::Remote { path, type_id }` when used as a field/element

`Remote` is what lets schemas reference other types without having to inline them everywhere; it also helps break cycles.

## Supported attributes

`#[derive(SerdeSchema)]` understands:

- **Container (`struct` / `enum`)**:
  - `#[serde(rename = "...")]`
  - `#[serde(rename_all = "...")]` (e.g. `snake_case`, `camelCase`, `PascalCase`, …)
  - `#[serde(transparent)]` (requires exactly one non-skipped field)
  - `#[serde(tag = "...")]` (enum only; internally tagged)
  - `#[serde(tag = "...", content = "...")]` (enum only; adjacently tagged)
- **Variant (`enum` variants)**:
  - `#[serde(rename = "...")]`
  - `#[serde(rename_all = "...")]`
  - `#[serde(skip)]` / `#[serde(skip_serializing)]`
- **Field**:
  - `#[serde(rename = "...")]`
  - `#[serde(skip)]` / `#[serde(skip_serializing)]`
  - `#[serde(flatten)]`
- **serde_sc-specific**:
  - `#[serde_sc(crate = "path::to::serde_sc")]` (useful if you re-export `serde_sc` under a different path)

## Notes / limitations

- **No untagged enums**: `#[serde(untagged)]` is not supported.
- **Internally tagged enums** (`#[serde(tag = "...")]`): only **unit** and **struct** variants are supported.
- **Flatten**: `#[serde(flatten)]` is intended for flattening a **struct-like** schema into its parent. At runtime, schema building will panic if the flattened field does not resolve to `TypeExpr::Struct { .. }`.
- **Recursion**: `T::type_expr()` uses a build `Context` and will panic if it detects direct recursive expansion of the same type. Using `Remote` references and `Registry::register::<T>()` is the recommended way to work with mutually-recursive graphs.
