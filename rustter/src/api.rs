use std::env;
use std::hash::{Hash, Hasher};

use diesel::*;
use diesel::prelude::*;
use diesel::pg::PgConnection;

use dotenv::dotenv;

use siphasher::sip::{SipHasher24};

use models::*;
use schema::*;

pub fn establish_connection() -> PgConnection
{ dotenv().ok()
; let database_url = env::var("DATABASE_URL")
      .expect("DATABASE_URL must be set")
; PgConnection::establish(&database_url)
  .expect(&format!("Error connecting to {}", database_url)) }

pub fn sign_up(screen_name : String, password : String)
{ let connection = establish_connection()
; match
    users::table
    .filter(users::screen_name.eq(&screen_name))
    .load::<User>(&connection).expect("Error loading users")
    .first()
  { Some(_) =>
      return ()
  , None =>
      () }
; let mut s = SipHasher24::new()
; password.hash(&mut s)
; let hash = s.finish()
; let new_users = vec![(users::screen_name.eq(&screen_name), users::hash.eq(hash as i64))]
; insert_into(users::table)
  .values(&new_users)
  .execute(&connection)
; () }
