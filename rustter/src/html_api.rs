use actix_web::{http, Result, HttpRequest, HttpResponse};
use askama::Template;

#[derive(Template)]
#[template(path = "sign-in.html")]
struct SignInTemplate {}

pub fn sign_in(_req: &HttpRequest) -> Result<HttpResponse>
{ let sign_in = SignInTemplate {}
; Ok(HttpResponse::build(http::StatusCode::OK)
    .content_type("text/html; charset=utf-8")
    .body(sign_in.render().unwrap())) }
