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

use std::collections::BTreeMap;

/// JSON value, modified from `serde_json::Value`
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    /// Represents a JSON null value.
    Null,

    /// Represents a JSON boolean.
    Bool(bool),

    /// Represents a JSON number, whether integer or floating point.
    ///
    /// for convenience, restrict to int
    Number(i64),

    /// Represents a JSON string.
    String(String),

    /// Represents a JSON array.
    Array(Vec<Value>),

    /// Represents a JSON object.
    ///
    /// By default the map is backed by a BTreeMap. Enable the `preserve_order`
    /// feature of serde_json to use IndexMap instead, which preserves
    /// entries in the order they are inserted into the map. In particular, this
    /// allows JSON data to be deserialized into a Value and serialized to a
    /// string while retaining the order of map keys in the input.
    ///
    Object(BTreeMap<String, Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => f.write_str("null"),
            Value::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
            Value::Number(n) => n.fmt(f),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Array(a) => {
                let mut sep = "";
                write!(
                    f,
                    "{}",
                    a.iter().fold(String::from("["), |acc, cur| {
                        let r = acc + sep + &cur.to_string();
                        sep = ",";
                        r
                    }) + "]"
                )
            }
            Value::Object(o) => {
                let mut sep = "";
                write!(
                    f,
                    "{}",
                    o.iter().fold(String::from("{"), |acc, (k, v)| {
                        let r = acc + sep + k + ":" + &v.to_string();
                        sep = ",";
                        r
                    }) + "}"
                )
            }
        }
    }
}
