#!/bin/bash

parse="../dist/build/parsechakoteya/parsechakoteya"

dir="$1"
if [ -z $dir ]; then
    dir="."
fi

for i in 101 $(seq 103 147) $(seq 149 277); do
    file=$dir/${i}.htm.new
    out=script${i}.txt

    echo "Processing ${file}"
    ${parse} <${file} >${out}
done
