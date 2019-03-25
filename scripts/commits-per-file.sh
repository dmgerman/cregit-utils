#!/bin/bash

FILE=$1

git log -w -p --follow --ignore-all-space --numstat --pretty=format:">>;$FILE;%H" "$FILE"
#| \cut -c 1-40 | sort
