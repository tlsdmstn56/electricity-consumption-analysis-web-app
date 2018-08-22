#!/bin/bash
LIBRARIES=()
APP_DIR="../src/app.R"
cat $APP_DIR | \
while read LINE; do
  if [[ "$LINE" =~ [[:space:]]*\#[[:space:]]*[a-zA-Z0-9]+ ]]; then
    continue
  elif [[ "$LINE" =~ ^library\(([a-zA-Z0-9]+)\) ]]; then
    # echo ${BASH_REMATCH[1]}
    LIBRARIES=( $LIBRARIES ${BASH_REMATCH[1]} )
  else
    break
  fi
done
echo ${LIBRARIES[1]}
for PKG in "${LIBRARIES[@]}"; do
  echo ${PKG}
done