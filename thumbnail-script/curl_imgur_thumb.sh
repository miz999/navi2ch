#!/bin/sh

curl -s $1 --create-dirs -o $2
curl -s $3 -o $4  "-H" "Authorization:Client-ID ebe3ee4157ab24a"

if [ -s $2 ]; then
    echo done $2
else
    rm $2
    echo zero file $2
fi

