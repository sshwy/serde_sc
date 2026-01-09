// Copyright 2024 Weiyao Huang
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Formatter,
};

use crate::value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ArrayType {
    pub elem: Box<TypeExpr>,
}

impl ArrayType {
    pub fn new(elem: TypeExpr) -> Self {
        Self {
            elem: Box::new(elem),
        }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        let oneline = self.elem.to_string_with(0, line_width) + "[]";
        if oneline.len() < line_width || indent == 0 {
            f.write_str(&oneline)
        } else {
            self.elem.write_fmt_with(f, indent, line_width)?;
            f.write_str("[]")
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Field {
    pub ty: TypeExpr,
    pub optional: bool,
}

impl Field {
    pub fn new(ty: TypeExpr) -> Self {
        Self {
            ty,
            optional: false,
        }
    }

    pub fn optional(ty: TypeExpr) -> Self {
        Self { ty, optional: true }
    }
}

impl From<TypeExpr> for Field {
    fn from(value: TypeExpr) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct StructType {
    pub record: BTreeMap<String, Field>,
}

impl StructType {
    pub fn new(record: BTreeMap<String, Field>) -> Self {
        Self { record }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        let lw2 = line_width.saturating_sub(4);
        let bs = self
            .record
            .iter()
            .map(|(k, v)| (k, (v.optional, v.ty.to_string_with(indent, lw2))))
            .collect();
        write!(f, "{}", StructTemplate { record: bs, indent })
    }
}

struct StructTemplate<'a> {
    record: BTreeMap<&'a String, (bool, String)>,
    indent: usize,
}

impl std::fmt::Display for StructTemplate<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str = " ".repeat(self.indent);
        let indent_ln = format!("\n{indent_str}");
        let indented = |s: &str| -> String { s.replace("\n", &indent_ln) };
        if self.indent > 0 {
            f.write_str("{\n")?;
            for (k, (optional, v)) in self.record.iter() {
                let opt = if *optional { "?" } else { "" };
                writeln!(f, "{indent_str}{k}{opt}: {v};", v = indented(v.as_str()))?;
            }
            f.write_str("}")
        } else {
            f.write_str("{ ")?;
            for (k, (optional, v)) in self.record.iter() {
                let opt = if *optional { "?" } else { "" };
                write!(f, "{k}{opt}: {v}; ")?;
            }
            f.write_str("}")
        }
    }
}

impl From<BTreeMap<String, Field>> for StructType {
    fn from(record: BTreeMap<String, Field>) -> Self {
        Self::new(record)
    }
}

impl From<BTreeMap<String, TypeExpr>> for StructType {
    fn from(record: BTreeMap<String, TypeExpr>) -> Self {
        Self::new(
            record
                .into_iter()
                .map(|(k, v)| (k, Field::new(v)))
                .collect(),
        )
    }
}

impl<const N: usize> From<[(String, Field); N]> for StructType {
    fn from(record: [(String, Field); N]) -> Self {
        Self::new(record.into_iter().collect())
    }
}

impl<const N: usize> From<[(String, TypeExpr); N]> for StructType {
    fn from(record: [(String, TypeExpr); N]) -> Self {
        Self::from(record.into_iter().collect::<BTreeMap<_, _>>())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct RecordType {
    pub key: Box<TypeExpr>,
    pub value: Box<TypeExpr>,
}

impl RecordType {
    pub fn new(key: TypeExpr, value: TypeExpr) -> Self {
        Self {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        f.write_str("Record<")?;
        self.key.write_fmt_with(f, indent, line_width)?;
        f.write_str(", ")?;
        self.value.write_fmt_with(f, indent, line_width)?;
        f.write_str(">")
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct UnionType {
    pub items: BTreeSet<TypeExpr>,
}

impl UnionType {
    pub fn new(items: BTreeSet<TypeExpr>) -> Self {
        Self { items }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        let lw2 = line_width.saturating_sub(4);
        let oneline = UnionTemplate {
            items: self
                .items
                .iter()
                .map(|v| v.to_string_with(0, line_width))
                .collect(),
            indent: 0,
        }
        .to_string();
        if oneline.len() < line_width || indent == 0 {
            f.write_str(&oneline)
        } else {
            let items = self
                .items
                .iter()
                .map(|v| v.to_string_with(indent, lw2))
                .collect();
            write!(f, "{}", UnionTemplate { items, indent })
        }
    }
}

impl From<BTreeSet<TypeExpr>> for UnionType {
    fn from(items: BTreeSet<TypeExpr>) -> Self {
        Self::new(items)
    }
}

impl<const N: usize> From<[TypeExpr; N]> for UnionType {
    fn from(items: [TypeExpr; N]) -> Self {
        Self::new(items.into_iter().collect())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SectType {
    pub items: BTreeSet<TypeExpr>,
}

impl SectType {
    pub fn new(items: BTreeSet<TypeExpr>) -> Self {
        Self { items }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        let lw2 = line_width.saturating_sub(4);
        let oneline = SectTemplate {
            items: self
                .items
                .iter()
                .map(|v| v.to_string_with(0, line_width))
                .collect(),
            indent: 0,
        }
        .to_string();
        if oneline.len() < line_width || indent == 0 {
            f.write_str(&oneline)
        } else {
            let items = self
                .items
                .iter()
                .map(|v| v.to_string_with(indent, lw2))
                .collect();
            write!(f, "{}", SectTemplate { items, indent })
        }
    }
}

impl From<BTreeSet<TypeExpr>> for SectType {
    fn from(items: BTreeSet<TypeExpr>) -> Self {
        Self::new(items)
    }
}

impl<const N: usize> From<[TypeExpr; N]> for SectType {
    fn from(items: [TypeExpr; N]) -> Self {
        Self::new(items.into_iter().collect())
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct TupleType {
    pub items: Vec<TypeExpr>,
}

impl TupleType {
    pub fn new(items: Vec<TypeExpr>) -> Self {
        Self { items }
    }

    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        let lw2 = line_width.saturating_sub(4);
        let oneline = TupleTemplate {
            items: self
                .items
                .iter()
                .map(|v| v.to_string_with(0, line_width))
                .collect(),
            indent: 0,
        }
        .to_string();
        if oneline.len() < line_width || indent == 0 {
            f.write_str(&oneline)
        } else {
            let items = self
                .items
                .iter()
                .map(|v| v.to_string_with(indent, lw2))
                .collect();
            write!(f, "{}", TupleTemplate { items, indent })
        }
    }
}

impl From<Vec<TypeExpr>> for TupleType {
    fn from(items: Vec<TypeExpr>) -> Self {
        Self::new(items)
    }
}

impl<const N: usize> From<[TypeExpr; N]> for TupleType {
    fn from(items: [TypeExpr; N]) -> Self {
        Self::new(items.into_iter().collect())
    }
}

/// TypeScript type representation.
///
/// ```rust
/// # use serde_sc_ts::expr::*;
/// let t = TypeExpr::Struct(
///     [
///         (String::from("name"), TypeExpr::String),
///         (
///             String::from("permissions"),
///             TypeExpr::Array(ArrayType::new(TypeExpr::Number)),
///         ),
///     ]
///     .into(),
/// );
///
/// // print the type with 80 line width limit and 4-space indent
/// eprintln!("{:80.4}", t);
/// ```
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum TypeExpr {
    /// name, id of another type
    /// `type name = {...}`
    Remote(String),
    /// `undefined`
    Undefined,
    /// `string`
    String,
    /// `number`
    Number,
    /// `boolean`
    Boolean,
    /// 具体的 value，例如 `type Name = 'hello';`
    Value(value::Value),
    /// `T[]`
    Array(ArrayType),
    /// `{ a: T, b: S }`
    Struct(StructType),
    /// `Record<K, V>`
    Record(RecordType),
    /// `[T, S]`
    Tuple(TupleType),
    /// ` T | S`
    Union(UnionType),
    /// ` T & S`
    Intersection(SectType),
    /// any
    Any,
}

impl TypeExpr {
    pub const fn null() -> Self {
        Self::Value(value::Value::Null)
    }
}

impl std::fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent = f.precision().unwrap_or(0);
        let line_width = f.width().unwrap_or(0);
        self.write_fmt_with(f, indent, line_width)
    }
}

struct TupleTemplate {
    items: Vec<String>,
    indent: usize,
}

impl std::fmt::Display for TupleTemplate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str = " ".repeat(self.indent);
        let indent_ln = format!("\n{indent_str}");
        let indented = |s: &str| -> String { s.replace("\n", &indent_ln) };
        if self.indent > 0 {
            f.write_str("[\n")?;
            for item in self.items.iter() {
                writeln!(f, "{indent_str}{item},", item = indented(item))?;
            }
            f.write_str("]")
        } else {
            write!(f, "[ {} ]", self.items.join(", "))
        }
    }
}

struct UnionTemplate {
    items: Vec<String>,
    indent: usize,
}

impl std::fmt::Display for UnionTemplate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str = " ".repeat(self.indent);
        let indent_ln = format!("\n{indent_str}");
        let indented = |s: &str| -> String { s.replace("\n", &indent_ln) };
        if self.indent > 0 {
            f.write_str("(\n")?;
            writeln!(f, "{indent_str}{item}", item = indented(&self.items[0]))?;
            for item in self.items.iter().skip(1) {
                writeln!(f, "{indent_str}| {item}", item = indented(item))?;
            }
            f.write_str(")")
        } else {
            write!(f, "( {} )", self.items.join(" | "))
        }
    }
}

struct SectTemplate {
    items: Vec<String>,
    indent: usize,
}

impl std::fmt::Display for SectTemplate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str = " ".repeat(self.indent);
        let indent_ln = format!("\n{indent_str}");
        let indented = |s: &str| -> String { s.replace("\n", &indent_ln) };
        if self.indent > 0 {
            f.write_str("(\n")?;
            writeln!(f, "{indent_str}{item}", item = indented(&self.items[0]))?;
            for item in self.items.iter().skip(1) {
                writeln!(f, "{indent_str}& {item}", item = indented(item))?;
            }
            f.write_str(")")
        } else {
            write!(f, "( {} )", self.items.join(" & "))
        }
    }
}

impl TypeExpr {
    /// Print the type expression w/o indent, trying to obey line width
    /// limitation.
    ///
    /// If `indent == 0`, print expression in one line.
    /// Otherwise, print expression with indent and WITHOUT ending new line.
    fn write_fmt_with(
        &self,
        f: &mut Formatter<'_>,
        indent: usize,
        line_width: usize,
    ) -> std::fmt::Result {
        match self {
            TypeExpr::Undefined => f.write_str("undefined"),
            TypeExpr::String => f.write_str("string"),
            TypeExpr::Number => f.write_str("number"),
            TypeExpr::Boolean => f.write_str("boolean"),
            TypeExpr::Any => f.write_str("any"),
            TypeExpr::Remote(n) => f.write_str(n),
            TypeExpr::Value(v) => write!(f, "{v}"),

            TypeExpr::Array(a) => a.write_fmt_with(f, indent, line_width),
            TypeExpr::Record(r) => r.write_fmt_with(f, indent, line_width),

            TypeExpr::Struct(t) => t.write_fmt_with(f, indent, line_width),
            TypeExpr::Tuple(t) => t.write_fmt_with(f, indent, line_width),
            TypeExpr::Union(t) => t.write_fmt_with(f, indent, line_width),
            TypeExpr::Intersection(t) => t.write_fmt_with(f, indent, line_width),
        }
    }

    fn to_string_with(&self, indent: usize, line_width: usize) -> String {
        struct DisplayWith<'a> {
            expr: &'a TypeExpr,
            indent: usize,
            line_width: usize,
        }

        impl std::fmt::Display for DisplayWith<'_> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.expr.write_fmt_with(f, self.indent, self.line_width)
            }
        }

        format!(
            "{}",
            DisplayWith {
                expr: self,
                indent,
                line_width
            }
        )
    }
}

impl TypeExpr {
    pub fn new_struct() -> Self {
        TypeExpr::Struct(StructType::new(BTreeMap::new()))
    }

    pub fn struct_insert(&mut self, k: String, v_type: TypeExpr) {
        let Self::Struct(s) = self else {
            panic!("invalid struct to insert")
        };
        s.record.insert(k, Field::new(v_type));
    }

    pub fn struct_merge(&mut self, v_type: TypeExpr) {
        let Self::Struct(s) = self else {
            panic!("invalid struct to merge")
        };
        let Self::Struct(t) = v_type else {
            panic!(
                "invalid struct to be merged {v_type:?}, it seems that you're merge a struct with another named struct. If you're using serde(flatten), try to use ts(inline), self = {self:?}"
            )
        };
        t.record.into_iter().for_each(|(k, v)| {
            s.record.insert(k, v);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typeexpr_struct_flat_and_multiline() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        let person = person();
        assert_eq!(
            person.to_string_with(0, 0),
            "{ name: string; permissions: number[]; }"
        );
        assert_eq!(
            person.to_string_with(4, 0),
            "{\n    name: string;\n    permissions: number[];\n}"
        );
    }

    #[test]
    fn test_typeexpr_struct_nested() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        fn person_ext() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("person"), Field::new(person())),
                    (String::from("age"), TypeExpr::Number.into()),
                    (String::from("zoo"), TypeExpr::Any.into()),
                ]
                .into(),
            ))
        }

        let person_ext = person_ext();
        assert_eq!(
            person_ext.to_string_with(4, 0),
            r#"{
    age: number;
    person: {
        name: string;
        permissions: number[];
    };
    zoo: any;
}"#
        );
    }

    #[test]
    fn test_typeexpr_record() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        fn person_ext() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("person"), Field::new(person())),
                    (String::from("age"), TypeExpr::Number.into()),
                    (String::from("zoo"), TypeExpr::Any.into()),
                ]
                .into(),
            ))
        }

        let person_map = TypeExpr::Record(RecordType::new(TypeExpr::String, person_ext()));
        assert_eq!(
            person_map.to_string_with(4, 0),
            r#"Record<string, {
    age: number;
    person: {
        name: string;
        permissions: number[];
    };
    zoo: any;
}>"#
        );
    }

    #[test]
    fn test_typeexpr_tuple() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        fn person_ext() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("person"), Field::new(person())),
                    (String::from("age"), TypeExpr::Number.into()),
                    (String::from("zoo"), TypeExpr::Any.into()),
                ]
                .into(),
            ))
        }

        fn person_map() -> TypeExpr {
            TypeExpr::Record(RecordType::new(TypeExpr::String, person_ext()))
        }

        let tuple = TypeExpr::Tuple(TupleType::new(vec![
            TypeExpr::Number,
            person_ext(),
            person_map(),
            TypeExpr::String,
        ]));

        assert_eq!(
            r#"[
    number,
    {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    },
    Record<string, {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    }>,
    string,
]"#,
            tuple.to_string_with(4, 0)
        );
    }

    #[test]
    fn test_typeexpr_union() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        fn person_ext() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("person"), Field::new(person())),
                    (String::from("age"), TypeExpr::Number.into()),
                    (String::from("zoo"), TypeExpr::Any.into()),
                ]
                .into(),
            ))
        }

        fn person_map() -> TypeExpr {
            TypeExpr::Record(RecordType::new(TypeExpr::String, person_ext()))
        }

        let union = TypeExpr::Union(
            [
                TypeExpr::Number,
                person_ext(),
                person_map(),
                TypeExpr::String,
            ]
            .into(),
        );
        assert_eq!(
            r#"(
    string
    | number
    | {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    }
    | Record<string, {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    }>
)"#,
            union.to_string_with(4, 0)
        );
    }

    #[test]
    fn test_typeexpr_intersection() {
        fn person() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("name"), TypeExpr::String.into()),
                    (
                        String::from("permissions"),
                        TypeExpr::Array(ArrayType::new(TypeExpr::Number)).into(),
                    ),
                ]
                .into(),
            ))
        }

        fn person_ext() -> TypeExpr {
            TypeExpr::Struct(StructType::new(
                [
                    (String::from("person"), Field::new(person())),
                    (String::from("age"), TypeExpr::Number.into()),
                    (String::from("zoo"), TypeExpr::Any.into()),
                ]
                .into(),
            ))
        }

        fn person_map() -> TypeExpr {
            TypeExpr::Record(RecordType::new(TypeExpr::String, person_ext()))
        }

        let sect = TypeExpr::Intersection(
            [
                TypeExpr::Number,
                person_ext(),
                person_map(),
                TypeExpr::String,
            ]
            .into(),
        );

        assert_eq!(
            r#"(
    string
    & number
    & {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    }
    & Record<string, {
        age: number;
        person: {
            name: string;
            permissions: number[];
        };
        zoo: any;
    }>
)"#,
            sect.to_string_with(4, 0)
        );
    }

    #[test]
    fn test_typeexpr_intersection_oneline() {
        let sect = TypeExpr::Intersection([TypeExpr::Number, TypeExpr::String].into());
        assert_eq!(sect.to_string_with(4, 80), "( string & number )");
    }
}
