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

pub mod models;
pub mod schema;
pub mod api;
pub mod json_api;
pub mod html_api;

use actix_web::{server, App, middleware};
use actix_web::middleware::session::SessionStorage;
use actix_redis::RedisSessionBackend;

pub fn main()
{ let sys = actix::System::new("rustter")
; server::new(||
    App::new()
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
