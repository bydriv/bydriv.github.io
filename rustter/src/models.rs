use diesel::data_types::PgTimestamp;

#[derive(Queryable)]
pub struct User
{ pub id : i64
, pub screen_name : String
, pub hash : i64 }

#[derive(Queryable)]
pub struct Following
{ pub id : i64
, pub user_id : i64
, pub following_user_id : i64 }

#[derive(Queryable)]
pub struct Status
{ pub id : i64
, pub user_id : i64
, pub text : String
, pub created_at : PgTimestamp }
