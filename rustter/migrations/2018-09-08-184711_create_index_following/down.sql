-- This file should undo anything in `up.sql`
ALTER TABLE followings DROP CONSTRAINT followings_following_user_id_user_id;
