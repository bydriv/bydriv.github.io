-- Your SQL goes here
ALTER TABLE followings ADD CONSTRAINT followings_following_user_id_user_id UNIQUE (following_user_id, user_id);
