#!/bin/bash

# Source directory (the folder containing original files)
SRC_DIR=~/Chalmers/TDA283/src

# Destination directory (current working directory)
DEST_DIR=~/Chalmers/TDA283/txt

# Loop through all files in the source directory
for file in "$SRC_DIR"/*; do
    # Skip if it's a directory
    if [ -d "$file" ]; then
        continue
    fi

    # Get just the filename (without path)
    filename=$(basename "$file")

    # Build the destination file path
    newfile="${DEST_DIR}/${filename}.txt"

    # Copy content to the new file
    cp "$file" "$newfile"

done

