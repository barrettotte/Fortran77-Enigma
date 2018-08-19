#!/bin/bash

# Simple bash script to compile specified .f file using gFortran Then, it will auto run the executable.
# Not very complex, just easier to run this one shell script instead of two separate commands.
# This will name the executable with the name of the source file.


function compilePrgm(){
    gfortran "${1}.f" -o "${1}.exe"
    if [ $? -eq 0 ]; then
        echo "Compile successful for ${1}.f"
        return 0;
    else
        echo "Error: Compile failed for ${1}.f"
        return 1;
    fi
}

function executePrgm(){
    echo "Executing ${1}.exe"
    echo ""
    ./"${1}.exe"
}



if [ $# -eq 0 ]; then
    echo "No arguments supplied."
else
    if [ -e $1 ]; then
        noExt="$(echo "$1" | rev | cut -d"/" -f1 | rev | cut -d"." -f1)"
        if compilePrgm $noExt; then 
            executePrgm $noExt
        else 
            echo "Error: Executable not created."
        fi
    else
        echo "Error: File not found."
    fi
fi