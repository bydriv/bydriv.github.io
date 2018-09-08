#[derive(Serialize, Deserialize)]
struct User
{ id : i64
, screen_name : String }

pub mod users
{ use actix_web::{HttpRequest}
; use serde_json
; use api
; use json_api
; pub fn list(_req: &HttpRequest) -> String
  { let users = api::users::list()
  ; serde_json::to_string(&users.into_iter().map(|user| json_api::User{id: user.id, screen_name: user.screen_name}).collect::<Vec<json_api::User>>()).expect("can't convert users to json") } }
