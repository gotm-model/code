#!/bin/sh

# if not set use the suggested source code installation directories
GOTM_BASE=${GOTM_BASE:=~/GOTM/code}
FABM_BASE=${FABM_BASE:=~/FABM/code}

# default Fortran compiler is gfortran - overide by setting compuiler like:
# export compiler=ifort
compiler=${compiler:=gfortran}

# NetCDF
# nf-config must be in the path and correpsond to the value of compiler
# try:
# nf-config --all

# Make install prefix configurable
install_prefix=${install_prefix:=~/local/gotm/$compiler}

# ready to configure
mkdir -p $compiler
cd $compiler
cmake $GOTM_BASE \
      -DGOTM_EMBED_VERSION=on \
      -DGOTM_USE_FABM=on \
      -DCMAKE_Fortran_COMPILER=$compiler \
      -DCMAKE_INSTALL_PREFIX=$install_prefix
cd ..
