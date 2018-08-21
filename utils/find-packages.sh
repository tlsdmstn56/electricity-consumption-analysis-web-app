#!/bin/bash
APP_PATH="../src/app.R"

if [ ! -f $APP_DIR ]; then
    echo "$APP_DIR not found!, aobrt"
    exit 1
fi

cat $APP_PATH | \
while read LINE;
do
  if [[ $LINE =~ ^[[:space:]]*#[[:space:]]*[a-zA-Z0-9]* ]]; then
    continue
  elif [[ $LINE =~ ^library\(([a-zA-Z0-9]+)\) ]]; then
    echo ${BASH_REMATCH[1]}
  else
    break
  fi
done