use std::env;
use std::hash::{Hash, Hasher};

use diesel::*;
use diesel::dsl::*;
use diesel::pg::PgConnection;

use dotenv::dotenv;

use siphasher::sip::{SipHasher24};

use models;
use schema;

/*
pub fn establish_connection() -> PgConnection
{ dotenv().ok()
; let database_url = env::var("DATABASE_URL")
      .expect("DATABASE_URL must be set")
; PgConnection::establish(&database_url)
  .expect(&format!("Error connecting to {}", database_url)) }
*/

pub fn find_user(connection : &PgConnection, id : i64) -> Option<models::User>
{ match
    schema::users::table
    .find(id)
    .get_result::<models::User>(connection)
  { Ok(user) =>
      Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash})
  , Err(_) =>
      None } }

pub fn find_user_by_screen_name(connection : &PgConnection, screen_name : String) -> Option<models::User>
{ match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .first::<models::User>(connection)
  { Ok(user) =>
      Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash})
  , _ =>
      None } }

pub fn sign_up(connection : &PgConnection, screen_name : String, password : String)
{ match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .first::<models::User>(connection)
  { Ok(_) =>
      return ()
  , _ =>
      () }
; let mut s = SipHasher24::new()
; password.hash(&mut s)
; let hash = s.finish()
; let new_users = vec![(schema::users::screen_name.eq(&screen_name), schema::users::hash.eq(hash as i64))]
; insert_into(schema::users::table)
  .values(&new_users)
  .execute(connection)
; () }

pub fn sign_in(connection : &PgConnection, screen_name : String, password : String) -> Option<models::User>
{ match
    schema::users::table
    .filter(schema::users::screen_name.eq(&screen_name))
    .first::<models::User>(connection)
  { Ok(user) =>
    { let mut s = SipHasher24::new()
    ; password.hash(&mut s)
    ; let hash = s.finish()
    ; if user.hash == (hash as i64)
      { Some(models::User {id: user.id, screen_name: user.screen_name.clone(), hash: user.hash}) }
      else
      { None } }
  , _ =>
      None } }

pub mod users
{ use super::*
; pub fn list(connection : &PgConnection) -> Vec<models::User>
  { schema::users::table
    .load::<models::User>(connection).expect("Error loading users") }
  pub fn follow(connection : &PgConnection, user_id : i64, following_user_id : i64)
  { if (is_followed(connection, user_id, following_user_id))
    { return () }
  ; let new_followings = vec![(schema::followings::user_id.eq(user_id), schema::followings::following_user_id.eq(following_user_id))]
  ; insert_into(schema::followings::table)
    .values(&new_followings)
    .execute(connection)
  ; () }
  pub fn unfollow(connection : &PgConnection, user_id : i64, following_user_id : i64)
  { if (!is_followed(connection, user_id, following_user_id))
    { return () }
  ; delete(schema::followings::table.filter(schema::followings::user_id.eq(user_id)).filter(schema::followings::following_user_id.eq(following_user_id)))
    .execute(connection)
  ; () }
  pub fn is_followed(connection : &PgConnection, user_id : i64, following_user_id : i64) -> bool
  { select(exists(schema::followings::table
    .filter(schema::followings::user_id.eq(user_id))
    .filter(schema::followings::following_user_id.eq(following_user_id))))
    .get_result(connection)
    .expect("Error loading users") }
  pub fn following_count(connection : &PgConnection, id : i64) -> i64
  { schema::followings::table
    .filter(schema::followings::user_id.eq(id))
    .select(count(schema::followings::id))
    .first(connection)
    .expect("Error loading users") }
  pub fn follower_count(connection : &PgConnection, id : i64) -> i64
  { schema::followings::table
    .filter(schema::followings::following_user_id.eq(id))
    .select(count(schema::followings::id))
    .first(connection)
    .expect("Error loading users") }
  pub fn following_user_ids(connection : &PgConnection, id : i64) -> Vec<i64>
  { schema::followings::table
    .filter(schema::followings::user_id.eq(id))
    .load::<models::Following>(connection)
    .expect("Error loading users")
    .into_iter()
    .map(|following| following.following_user_id)
    .collect() } }

pub fn timeline(connection : &PgConnection, id : i64) -> Vec<models::Status>
{ let following_user_ids = users::following_user_ids(connection, id)
; let mut statuses = vec![]
; statuses.extend
    (schema::statuses::table
    .filter(schema::statuses::user_id.eq(id))
    .load::<models::Status>(connection)
    .expect("Error loading users"))
; for following_user_id in following_user_ids
  { statuses.extend
      (schema::statuses::table
      .filter(schema::statuses::user_id.eq(following_user_id))
      .load::<models::Status>(connection)
      .expect("Error loading users")) }
; statuses }
