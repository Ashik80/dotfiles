#!/usr/bin/env bash

docker images -a --format table | awk '/<none>/ {print $3}' | xargs docker rmi
