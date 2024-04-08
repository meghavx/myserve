#!/bin/bash

set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL

  create table service_user (
    user_id varchar(50),
    joined timestamptz not null,
    password_argon2id varchar(194),
    primary key (user_id)
  );

  create table auth_token (
    auth_token uuid,
    created_at timestamptz not null,
    created_by varchar(50),
    foreign key (created_by) references service_user(user_id),
    primary key(auth_token)
  );

  create table request_log (
    log_id uuid,
    client_address varchar(50) not null,
    request_headers json,
    path varchar(200) not null,
    primary key (log_id)
  );
  
EOSQL
