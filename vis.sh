#!/bin/bash

set -e

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <output_dir>" >&2
  exit 1
fi

output_dir=$1

mkdir -p "$output_dir/vis"

for file in $output_dir/*.dot; do
  name="$(basename "$file")"
  dot "$file" -Tpng -o "$output_dir/vis/$name.png"
done
