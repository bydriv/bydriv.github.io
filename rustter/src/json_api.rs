use std::sync::Arc;

use actix_web::{HttpRequest, Form};
use actix_web::middleware::session::{self, RequestSession};

use diesel::pg::PgConnection;
use diesel::r2d2;

use serde_json;

use api;
use json_api;

#[derive(Serialize, Deserialize)]
pub struct User
{ id : i64
, screen_name : String }

#[derive(Serialize, Deserialize)]
pub struct SignInParams
{ screen_name : String
, password : String }

pub fn sign_up((req, params): (HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>, Form<SignInParams>)) -> String
{ let connection : &PgConnection = &req.state().get().unwrap()
; api::sign_up(connection, params.screen_name.clone(), params.password.clone())
; if let Some(user) = api::sign_in(connection, params.screen_name.clone(), params.password.clone())
  { req.session().set::<i64>("user_id", user.id); "true".to_owned() }
  else
  { "false".to_owned() } }

pub fn sign_in((req, params): (HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>, Form<SignInParams>)) -> String
{ let connection : &PgConnection = &req.state().get().unwrap()
; if let Some(user) = api::sign_in(connection, params.screen_name.clone(), params.password.clone())
  { req.session().set::<i64>("user_id", user.id); "true".to_owned() }
  else
  { "false".to_owned() } }

pub mod users
{ use super::*
; pub fn list(req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> String
  { match req.session().get::<i64>("user_id")
    { Ok(Some(user_id)) => println!("{}", user_id)
    , Ok(None) => println!("Ok(None)")
    , Err(_) => println!("Err(_)") }
  ; let connection : &PgConnection = &req.state().get().unwrap()
  ; let users = api::users::list(connection)
  ; serde_json::to_string(&users.into_iter().map(|user| json_api::User{id: user.id, screen_name: user.screen_name}).collect::<Vec<json_api::User>>()).expect("can't convert users to json") } }
