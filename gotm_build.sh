#!/bin/bash

if [ "$1" != "" ]; then
   action=$1
   echo "doing a "$action
else
   action=install
fi

compiler=gfortran

np=-j8
cd $compiler
make $np $action
cd ..
