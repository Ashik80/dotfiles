#!/usr/bin/env bash

database=${DATABASE}

if [[ -z $database ]]; then
    echo "arg DATABASE not provided"
    exit 1
fi

file=${FILE}

if [[ -z $file ]]; then
    echo "arg FILE not provided"
    exit 1
fi

PGPASSWORD=postgres psql -U postgres -d $database -f $file
