use serde_sc::{SerdeSchema, registry::Registry};

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
    println!("{:?}", Person::type_expr());
    let mut r = Registry::new();
    r.register::<Person>();
    println!("r = {:?}", r);
}
