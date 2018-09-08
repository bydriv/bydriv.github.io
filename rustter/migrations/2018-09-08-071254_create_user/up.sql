-- Your SQL goes here
CREATE TABLE users
  ( id BIGSERIAL PRIMARY KEY
  , screen_name TEXT NOT NULL UNIQUE
  , hash BIGINT NOT NULL )
