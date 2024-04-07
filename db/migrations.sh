#!/bin/bash

set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL

  create table member (
    email varchar(100),
    first_name varchar(50) not null,
    last_name varchar(50) not null,
    date_of_birth date not null,
    permanent boolean not null,
    correspondence_address varchar(200) null,
    primary key(email)
  );
  
  create index if not exists idx_member_date_of_birth on member (date_of_birth);
  
  create index if not exists idx_member_permanent on member (permanent);

  insert into member (email, first_name, last_name, date_of_birth, permanent, correspondence_address) values
      ('peter.pan@email.com', 'Peter', 'Pan', '1970-11-19', true, null),
      ('john.doe@email.com', 'John', 'Doe', '1966-07-30', true, null),
      ('brenda.song@email.com', 'Brenda', 'Song', '1992-01-25', false, null),
      ('jane.doe@email.com', 'Jane', 'Doe', '1970-04-01', true, '123 Main St., Townsville'),
      ('walter.white@iucn.org', 'Walter', 'White', '1958-09-07', true, '308 Negra Arroyo Ln, Albuquerque NM');

  create table auth_token (
    auth_token char(8),
    created timestamp,
    primary key(auth_token)
  );

  insert into auth_token (auth_token, created) values ('default0', now());
  
EOSQL
