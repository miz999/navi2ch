#!/bin/sh
mkdir -p `dirname $5`
curl -s --create-dirs --dump-header "$5".header "$1?url=$2&w=$3&h=$4" --create-dirs -o "$5".jpg
#curl -s "$1?url=$2&w=$3&h=$4" --create-dirs -o "$5"
#curl -s "$1?url=$2&w=$3&h=$4" -dump-header -o "$5".header 
echo done $2
