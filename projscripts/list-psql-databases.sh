#!/usr/bin/env bash

PGPASSWORD=postgres psql -U postgres -c 'select datname from pg_database' | head -n -2 | tail -n +3 | sed 's/^\s\+//'
