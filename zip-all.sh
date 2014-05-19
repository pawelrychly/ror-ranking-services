#!/bin/bash

DST="packages-to-publish"

rm -rf "$DST"

mkdir "$DST"

for i in ./* ; do
  if [ -d "$i" ]; then
    if [ "$(basename "$i")" != "$DST" ]; then
      zip -r "$DST/$(basename "$i").zip" "$i"
    fi
  fi
done
