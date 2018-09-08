use actix_web::{HttpRequest, Form};
use actix_web::middleware::session::{self, RequestSession};
use api;

#[derive(Serialize, Deserialize)]
pub struct User
{ id : i64
, screen_name : String }

#[derive(Serialize, Deserialize)]
pub struct SignInParams
{ screen_name : String
, password : String }

pub fn sign_in((req, params): (HttpRequest, Form<SignInParams>)) -> String
{ if let Some(user) = api::sign_in(params.screen_name.clone(), params.password.clone())
  { req.session().set::<i64>("user_id", user.id); "true".to_owned() }
  else
  { "false".to_owned() } }

pub mod users
{ use actix_web::{HttpRequest}
; use serde_json
; use actix_web::middleware::session::{self, RequestSession}
; use api
; use json_api
; pub fn list(req: &HttpRequest) -> String
  { match req.session().get::<i64>("user_id")
    { Ok(Some(user_id)) => println!("{}", user_id)
    , Ok(None) => println!("Ok(None)")
    , Err(_) => println!("Err(_)") }
  ; let users = api::users::list()
  ; serde_json::to_string(&users.into_iter().map(|user| json_api::User{id: user.id, screen_name: user.screen_name}).collect::<Vec<json_api::User>>()).expect("can't convert users to json") } }
