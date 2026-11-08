#!/usr/bin/env bash

database=${DATABASE}

if [[ -z $database ]]; then
    echo "arg DATABASE not provided"
    exit 1
fi

PGPASSWORD=postgres psql -U postgres -d $database -c "select table_name from information_schema.tables where table_schema = 'public'" -tA
