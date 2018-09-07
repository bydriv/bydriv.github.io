extern crate actix_web;
use actix_web::{server, App, HttpRequest};

#[macro_use]
extern crate diesel;
extern crate dotenv;

use diesel::prelude::*;
use diesel::pg::PgConnection;
use dotenv::dotenv;
use std::env;

#[derive(Queryable)]
pub struct TestTable {
  pub id: i32,
  pub text: String,
}

table! {
  test_table (id) {
    id -> Integer,
    text -> Text,
  }
}

pub fn establish_connection() -> PgConnection {
  dotenv().ok();

  let database_url = env::var("DATABASE_URL")
      .expect("DATABASE_URL must be set");
  PgConnection::establish(&database_url)
  .expect(&format!("Error connecting to {}", database_url))
}

//fn index(_req: &HttpRequest) -> &'static str {
fn index(_req: &HttpRequest) -> String {
  let connection = establish_connection();
  let entry = test_table::table.find(1).get_result::<TestTable>(&connection);
  match entry {
    Ok(e) => e.text,
    Err(_) => "error".to_owned(),
  }
}

fn main() {
  server::new(|| App::new().resource("/", |r| r.f(index)))
    .bind("127.0.0.1:8088")
    .unwrap()
    .run();
}
