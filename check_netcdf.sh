#!/bin/bash

# For Linux and Mac users only. Windows NetCDF libraries are included.

command -v nf-config >/dev/null 2>&1 || \
   { echo >&2 "nf-config is not available.  Please make it available (adjust PATH or install)."; exit 1; }

echo ""
echo "Checking if CMake can find NetCDF compile and link information ..."
echo ""
cmake -P extern/flexout/cmake/Modules/FindNetCDF.cmake
echo "The above MUST include - Found NetCDF - to successfully continue"
echo ""

