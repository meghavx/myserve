#!/bin/bash

env SERVER_PORT=8080 \
    PG_CONNECT_STRING='host=localhost port=54321 user=postgres password=password dbname=devdb' \
    AUTH_TOKEN_TIMEOUT_SECONDS=120 \
    stack run myserve
