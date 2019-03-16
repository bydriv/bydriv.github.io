extern crate proptest;
extern crate proptest_derive;
extern crate serde;
extern crate serde_json;
extern crate valico;

use proptest::prelude::*;
use proptest_derive::*;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Arbitrary, Debug)]
struct Person {
    name: String,
    age: u8,
    phones: Vec<String>,
}

proptest! {
    #[test]
    fn test_person(p: Person) {
        use serde_json::Value;
        use valico::json_schema;
        use std::fs::File;

        let json_v4_schema: Value = serde_json::from_reader(File::open("schema.json").unwrap()).unwrap();
        let mut scope = json_schema::Scope::new();
        let schema = scope.compile_and_return(json_v4_schema.clone(), false).unwrap();

        let value = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();

        assert!(schema.validate(&value).is_valid());
    }
}
