#!/usr/bin/env bash

ffplay -fflags nobuffer -flags low_delay -framedrop -an /dev/video0
