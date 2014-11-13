#!/bin/bash

if [ -z "$1" ]; then
    echo "usage: $0 dirname"
    echo "dirname is a directory containing images that we will (recursively) build histograms for"
    exit 1
fi

make all

for file in $(find "$1"); do
    echo "$file"
    ./calc_histogram "$file" > /dev/null 2> /dev/null
done
