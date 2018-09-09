use std::sync::Arc;

use actix_web::{http, Result, HttpRequest, HttpResponse};
use actix_web::middleware::session::{RequestSession};
use askama::Template;

use diesel::pg::PgConnection;
use diesel::r2d2;

use api;
use json_api;

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate
{ screen_name : String
, statuses : Vec<json_api::Status> }

#[derive(Template)]
#[template(path = "sign-up.html")]
struct SignUpTemplate {}

#[derive(Template)]
#[template(path = "sign-in.html")]
struct SignInTemplate {}

#[derive(Template)]
#[template(path = "user-show.html")]
struct UserShowTemplate
{ screen_name : String
, following_count : i64
, follower_count : i64
, you_followed : bool
, you_are_followed : bool }

#[derive(Template)]
#[template(path = "not-found.html")]
struct NotFoundTemplate {}

pub fn index(req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
{ match req.session().get::<i64>("user_id")
  { Ok(Some(user_id)) =>
    { let connection : &PgConnection = &req.state().get().unwrap()
    ; let user = api::find_user(connection, user_id).unwrap()
    ; let index = IndexTemplate { screen_name: user.screen_name, statuses: api::timeline(connection, user.id).into_iter().map(|status| { let user = api::find_user(connection, status.user_id).unwrap(); json_api::Status { id: status.id, user: json_api::User { id: user.id, screen_name: user.screen_name }, text: status.text } }).collect() }
    ; Ok(HttpResponse::build(http::StatusCode::OK)
        .content_type("text/html; charset=utf-8")
        .body(index.render().unwrap())) }
  , _ =>
      sign_in(req) } }

pub fn sign_up(_req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
{ let sign_up = SignUpTemplate {}
; Ok(HttpResponse::build(http::StatusCode::OK)
    .content_type("text/html; charset=utf-8")
    .body(sign_up.render().unwrap())) }

pub fn sign_in(_req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
{ let sign_in = SignInTemplate {}
; Ok(HttpResponse::build(http::StatusCode::OK)
    .content_type("text/html; charset=utf-8")
    .body(sign_in.render().unwrap())) }

pub mod users
{ use super::*
; pub fn show(req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
  { match req.session().get::<i64>("user_id")
    { Ok(Some(user_id)) =>
      { let connection : &PgConnection = &req.state().get().unwrap()
      ; let user = api::find_user(connection, user_id).unwrap()
      ; let showing_user = api::find_user_by_screen_name(connection, req.match_info().get("screen_name").unwrap().to_string())
      ; match showing_user
        { Some(showing_user) =>
          { let user_show = UserShowTemplate {screen_name: showing_user.screen_name, following_count: api::users::following_count(connection, showing_user.id), follower_count: api::users::follower_count(connection, showing_user.id), you_followed: api::users::is_followed(connection, user.id, showing_user.id), you_are_followed: api::users::is_followed(connection, showing_user.id, user.id) }
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(user_show.render().unwrap())) }
        , None =>
          { let not_found = NotFoundTemplate {}
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(not_found.render().unwrap())) } } }
    , _ =>
      { let connection : &PgConnection = &req.state().get().unwrap()
      ; let showing_user = api::find_user_by_screen_name(connection, req.match_info().get("screen_name").unwrap().to_string())
      ; match showing_user
        { Some(showing_user) =>
          { let user_show = UserShowTemplate {screen_name: showing_user.screen_name, following_count: api::users::following_count(connection, showing_user.id), follower_count: api::users::follower_count(connection, showing_user.id), you_followed: false, you_are_followed: false }
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(user_show.render().unwrap())) }
        , None =>
          { let not_found = NotFoundTemplate {}
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(not_found.render().unwrap())) } } } } }
  pub fn follow(req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
  { match req.session().get::<i64>("user_id")
    { Ok(Some(user_id)) =>
      { let connection : &PgConnection = &req.state().get().unwrap()
      ; let showing_user = api::find_user_by_screen_name(connection, req.match_info().get("screen_name").unwrap().to_string())
      ; match showing_user
        { Some(showing_user) =>
          { api::users::follow(connection, user_id, showing_user.id)
          ; index(req) }
        , None =>
          { let not_found = NotFoundTemplate {}
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(not_found.render().unwrap())) } } }
    , _ =>
        sign_in(req) } }
  pub fn unfollow(req: &HttpRequest<Arc<r2d2::Pool<r2d2::ConnectionManager<PgConnection>>>>) -> Result<HttpResponse>
  { match req.session().get::<i64>("user_id")
    { Ok(Some(user_id)) =>
      { let connection : &PgConnection = &req.state().get().unwrap()
      ; let showing_user = api::find_user_by_screen_name(connection, req.match_info().get("screen_name").unwrap().to_string())
      ; match showing_user
        { Some(showing_user) =>
          { api::users::unfollow(connection, user_id, showing_user.id)
          ; index(req) }
        , None =>
          { let not_found = NotFoundTemplate {}
          ; Ok(HttpResponse::build(http::StatusCode::OK)
              .content_type("text/html; charset=utf-8")
              .body(not_found.render().unwrap())) } } }
    , _ =>
        sign_in(req) } } }
