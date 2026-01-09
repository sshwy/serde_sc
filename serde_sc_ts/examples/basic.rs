use serde_sc::{SerdeSchema, registry::Registry};
use serde_sc_ts::{DeclWorld, Flavor};

#[derive(SerdeSchema)]
#[allow(dead_code)]
struct Person {
    name: String,
    addr: Address,
    #[serde(flatten)]
    info: Box<Info>,
}

#[allow(dead_code)]
#[derive(SerdeSchema)]
struct Address {
    street: String,
    city: String,
    zip: String,
}

#[derive(SerdeSchema)]
#[allow(dead_code)]
struct Info {
    description: String,
    father: Option<Person>,
    mother: Option<Person>,
}

fn main() {
    let mut r = Registry::new();
    r.register::<Person>();
    let w = DeclWorld::new(&r);
    println!(
        "{}",
        w.to_export_statement(std::any::TypeId::of::<Person>(), Flavor::Deserialize)
    );
}
