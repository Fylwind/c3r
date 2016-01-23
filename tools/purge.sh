#!/bin/sh
set -eu
sqlite3 "$1" <<EOF
DELETE FROM deletes;
DELETE FROM log;
DELETE FROM log;
DELETE FROM misc_stream_msgs;
DELETE FROM statuses;
DELETE FROM url_cache;
DELETE FROM user_updates;
DELETE FROM users;
EOF
