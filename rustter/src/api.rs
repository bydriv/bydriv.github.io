use std::env;
use std::hash::{Hash, Hasher};

use diesel::*;
use diesel::dsl::*;
use diesel::pg::PgConnection;

use dotenv::dotenv;

use siphasher::sip::{SipHasher24};

use models;
use schema;

pub fn establish_connection() -> PgConnection
{ dotenv().ok()
; let database_url = env::var("DATABASE_URL")
      .expect("DATABASE_URL must be set")
; PgConnection::establish(&database_url)
  .expect(&format!("Error connecting to {}", database_url)) }

pub fn find_user(id : i64) -> Option<models::User>
{ let connection = establish_connection()
; match
    schema::users::table
    .find(id)
    .get_result::<models::User>(&connection)
  { Ok(user) =>
      Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash})
  , Err(_) =>
      None } }

pub fn find_user_by_screen_name(screen_name : String) -> Option<models::User>
{ let connection = establish_connection()
; match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .load::<models::User>(&connection).expect("Error loading users")
    .first()
  { Some(user) =>
      Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash})
  , None =>
      None } }

pub fn sign_up(screen_name : String, password : String)
{ let connection = establish_connection()
; match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .load::<models::User>(&connection).expect("Error loading users")
    .first()
  { Some(_) =>
      return ()
  , None =>
      () }
; let mut s = SipHasher24::new()
; password.hash(&mut s)
; let hash = s.finish()
; let new_users = vec![(schema::users::screen_name.eq(&screen_name), schema::users::hash.eq(hash as i64))]
; insert_into(schema::users::table)
  .values(&new_users)
  .execute(&connection)
; () }

pub fn sign_in<'a>(screen_name : String, password : String) -> Option<models::User>
{ let connection = establish_connection()
; match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .load::<models::User>(&connection).expect("Error loading users")
    .first()
  { Some(user) =>
    { let mut s = SipHasher24::new()
    ; password.hash(&mut s)
    ; let hash = s.finish()
    ; if user.hash == (hash as i64)
      { Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash}) }
      else
      { None } }
  , None =>
      None } }

pub mod users
{ use super::*
; pub fn list() -> Vec<models::User>
  { let connection = establish_connection()
  ; schema::users::table
    .load::<models::User>(&connection).expect("Error loading users") }
  pub fn is_followed(user_id : i64, following_user_id : i64) -> bool
  { let connection = establish_connection()
  ; select(exists(schema::followings::table
    .filter(schema::followings::user_id.eq(user_id))
    .filter(schema::followings::following_user_id.eq(following_user_id))))
    .get_result(&connection)
    .expect("Error loading users") }
  pub fn following_count(id : i64) -> i64
  { let connection = establish_connection()
  ; schema::followings::table
    .filter(schema::followings::user_id.eq(id))
    .select(count(schema::followings::id))
    .first(&connection)
    .expect("Error loading users") }
  pub fn follower_count(id : i64) -> i64
  { let connection = establish_connection()
  ; schema::followings::table
    .filter(schema::followings::following_user_id.eq(id))
    .select(count(schema::followings::id))
    .first(&connection)
    .expect("Error loading users") } }
