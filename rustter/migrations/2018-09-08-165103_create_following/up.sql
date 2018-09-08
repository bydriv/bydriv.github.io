-- Your SQL goes here
CREATE TABLE followings
  ( id BIGSERIAL PRIMARY KEY
  , user_id BIGINT NOT NULL
  , following_user_id BIGINT NOT NULL );
ALTER TABLE followings ADD CONSTRAINT followings_user_id_following_user_id UNIQUE (user_id, following_user_id);
