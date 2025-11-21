#!/bin/bash

# Define the input CSV file
input_file="rerun.csv"

# Read the CSV file and process each row
awk -F, 'NR > 1 { print $1, $2, $3 }' "$input_file" | while read -r mod iter flag; do

  mod=$(echo "$mod" | sed 's/^"\(.*\)"$/\1/')
  flag=$(echo "$flag" | sed 's/^"\(.*\)"$/\1/')
  
  echo "$mod $iter $flag"
  sbatch run-single.sh $mod $iter $flag
done
