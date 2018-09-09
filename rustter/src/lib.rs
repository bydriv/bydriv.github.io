extern crate actix;
extern crate actix_web;
#[macro_use]
extern crate diesel;
extern crate dotenv;
extern crate siphasher;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;
extern crate actix_redis;
#[macro_use]
extern crate askama;
extern crate r2d2;

pub mod models;
pub mod schema;
pub mod api;
pub mod json_api;
pub mod html_api;

use std::sync::Arc;
use std::env;

use actix_web::{server, App};
use actix_web::middleware::session::SessionStorage;
use actix_redis::RedisSessionBackend;

use diesel::pg::PgConnection;
use diesel::r2d2::ConnectionManager;

use dotenv::dotenv;

pub fn main()
{ dotenv().ok()
; let database_url = env::var("DATABASE_URL")
      .expect("DATABASE_URL must be set")
; let manager = ConnectionManager::<PgConnection>::new(database_url);
; let pool = Arc::new(r2d2::Pool::builder().max_size(15).build(manager).unwrap())
; let sys = actix::System::new("rustter")
; server::new(move ||
    App::with_state(pool.clone())
    .middleware(SessionStorage::new(RedisSessionBackend::new("127.0.0.1:6379", &[0; 32]).ttl(60)))
    .resource("/sign-up.json", |r| r.with(json_api::sign_up))
    .resource("/sign-in.json", |r| r.with(json_api::sign_in))
    .resource("/users/list.json", |r| r.f(json_api::users::list))
    .resource("/sign-up.html", |r| r.f(html_api::sign_up))
    .resource("/user/{screen_name}", |r| r.f(html_api::users::show))
    .resource("/follow/{screen_name}", |r| r.f(html_api::users::follow))
    .resource("/unfollow/{screen_name}", |r| r.f(html_api::users::unfollow))
    .resource("/", |r| r.f(html_api::index)))
  .bind("127.0.0.1:8088")
  .unwrap()
  .start()
; sys.run()
; () }
