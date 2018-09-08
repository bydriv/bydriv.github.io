use actix_web::{http, Result, HttpRequest, HttpResponse};
use actix_web::middleware::session::{self, RequestSession};
use askama::Template;

use api;

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate
{ screen_name : String }

#[derive(Template)]
#[template(path = "sign-up.html")]
struct SignUpTemplate {}

#[derive(Template)]
#[template(path = "sign-in.html")]
struct SignInTemplate {}

pub fn index(req: &HttpRequest) -> Result<HttpResponse>
{ match req.session().get::<i64>("user_id")
  { Ok(Some(user_id)) =>
    { let user = api::find_user(user_id).unwrap()
    ; let index = IndexTemplate {screen_name: user.screen_name}
    ; Ok(HttpResponse::build(http::StatusCode::OK)
        .content_type("text/html; charset=utf-8")
        .body(index.render().unwrap())) }
  , _ =>
      sign_in(req) } }

pub fn sign_up(_req: &HttpRequest) -> Result<HttpResponse>
{ let sign_up = SignUpTemplate {}
; Ok(HttpResponse::build(http::StatusCode::OK)
    .content_type("text/html; charset=utf-8")
    .body(sign_up.render().unwrap())) }

pub fn sign_in(_req: &HttpRequest) -> Result<HttpResponse>
{ let sign_in = SignInTemplate {}
; Ok(HttpResponse::build(http::StatusCode::OK)
    .content_type("text/html; charset=utf-8")
    .body(sign_in.render().unwrap())) }
