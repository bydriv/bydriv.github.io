use std::sync::Arc;

use actix_web::{http, HttpRequest, HttpResponse, Form};
use actix_web::middleware::session::{RequestSession};

use diesel::pg::PgConnection;
use diesel::r2d2;

use serde_json;

use api;
use json_api;

#[derive(Serialize, Deserialize)]
pub struct User
{ pub id : i64
, pub screen_name : String }

#[derive(Serialize, Deserialize)]
pub struct SignInParams
{ pub screen_name : String
, pub password : String }

#[derive(Serialize, Deserialize)]
pub struct Status
{ pub id : i64
, pub user : User
, pub text : String }

pub fn sign_up((req, params): (HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>, Form<SignInParams>)) -> HttpResponse
{ let connection : &PgConnection = &req.state().get().unwrap()
; api::sign_up(connection, params.screen_name.clone(), params.password.clone())
; if let Some(user) = api::sign_in(connection, params.screen_name.clone(), params.password.clone())
  { req.session().set::<i64>("user_id", user.id)
  ; HttpResponse::Found()
    .header(http::header::LOCATION, "/")
    .finish() }
  else
  { HttpResponse::Found()
    .header(http::header::LOCATION, "/")
    .finish() } }

pub fn sign_in((req, params): (HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>, Form<SignInParams>)) -> HttpResponse
{ let connection : &PgConnection = &req.state().get().unwrap()
; if let Some(user) = api::sign_in(connection, params.screen_name.clone(), params.password.clone())
  { req.session().set::<i64>("user_id", user.id)
  ; HttpResponse::Found()
    .header(http::header::LOCATION, "/")
    .finish() }
  else
  { HttpResponse::Found()
    .header(http::header::LOCATION, "/")
    .finish() } }

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
