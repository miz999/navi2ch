#!/bin/sh
curl -s "$1" --create-dirs -o $2

if [ -s $2 ]; then
    echo done $1
else
    echo zero file $1 $2
    rm $2
fi

