#[macro_use]
extern crate diesel;
extern crate dotenv;
extern crate siphasher;

pub mod models;
pub mod schema;
pub mod api;

pub fn f() {
  println!("hello world");
}
