#!/bin/sh

# if not set in the environment point to the code here
#export GOTM_BASE=~/GOTM/code 
#export FABM_BASE=~/FABM/code

compiler=gfortran
mkdir -p $compiler
cd $compiler
cmake $GOTM_BASE/src \
      -DGOTM_USE_FLEXIBLE_OUTPUT=on \
      -DGOTM_USE_FABM=on \
      -DFABM_BASE=$FABM_BASE/ \
      -DCMAKE_Fortran_COMPILER=$compiler \
      -DCMAKE_INSTALL_PREFIX=~/local/gotm/$compiler
cd ..
