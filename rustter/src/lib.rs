extern crate actix_web;
#[macro_use]
extern crate diesel;
extern crate dotenv;
extern crate siphasher;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

pub mod models;
pub mod schema;
pub mod api;
pub mod json_api;

use actix_web::{server, App};

pub fn main()
{ server::new(|| App::new().resource("/users/list.json", |r| r.f(json_api::users::list)))
  .bind("127.0.0.1:8088")
  .unwrap()
  .run()
; () }
