#$Id: Rules.make,v 1.4 2001-05-23 11:32:10 gotm Exp $

SHELL   = /bin/sh

VERSION = 2
PATCHLEVEL = 3
SUBLEVEL = 1

VER     = $(VERSION).$(PATCHLEVEL).$(SUBLEVEL)

# The compilation mode is obtained from $COMPILATION_MODE - 
# default production - else debug or profiling
ifndef COMPILATION_MODE
compilation=production
else
compilation=$(COMPILATION_MODE)
endif

DEFINES=-DNUDGE_VEL
DEFINES=

# What do we include in this compilation
NetCDF=false
NetCDF=true
SEDIMENT=false
SEDIMENT=true
SEAGRASS=false
SEAGRASS=true

FEATURES	=
FEATURE_LIBS	=
EXTRA_LIBS	=
INCDIRS		=
LDFLAGS		=

# If we want NetCDF - where are the include files and the library
ifdef NETCDFINC
INCDIRS		+= -I$(NETCDFINC)
endif
ifdef NETCDFLIBNAME
NETCDFLIB	= $(NETCDFLIBNAME)
else
NETCDFLIB	= -lnetcdf
ifdef NETCDFLIBDIR
LDFLAGS		+= -L$(NETCDFLIBDIR)
endif
endif

#
# False targets.
#
.PHONY: dummy

# Top of this version of GOTM.
ifndef TOPDIR
TOPDIR  := $(HOME)/gotm
endif

# The Fortran compiler is determined from the EV FORTRAN_COMPILER - options 
# sofar NAG(linux), FUJITSU(Linux), DECF90 (OSF1 and likely Linux on alpha),
# SunOS.

CPP	= /lib/cpp

# Here you can put defines for the [c|f]pp - some will also be set depending
# on compilation mode.
ifeq ($(NetCDF),true)
DEFINES += -DNETCDF_FMT
EXTRA_LIBS += $(NETCDFLIB)
endif
ifeq ($(SEDIMENT),true)
DEFINES += -DSEDIMENT
FEATURES += extras/sediment
FEATURE_LIBS += -lsediment$(buildtype)
endif
ifeq ($(SEAGRASS),true)
DEFINES += -DSEAGRASS
FEATURES += extras/seagrass
FEATURE_LIBS += -lseagrass$(buildtype)
endif

# Directory related settings.

ifndef BINDIR
BINDIR	= $(TOPDIR)/bin
endif

ifndef LIBDIR
LIBDIR	= $(TOPDIR)/lib/$(FORTRAN_COMPILER)
endif

ifndef MODDIR
MODDIR	= $(TOPDIR)/modules
MODDIR	= $(TOPDIR)/modules/$(FORTRAN_COMPILER)
endif
INCDIRS	+= -I/usr/local/include -I$(TOPDIR)/include -I$(MODDIR)

# Normaly this should not be changed - unless you want something very specific.

#ifneq (compiler_included,true)
#include $(TOPDIR)/compilers/compiler.$(FORTRAN_COMPILER)
#endif

# Set options for the NAG Fortran compiler.
ifeq ($(FORTRAN_COMPILER),NAG)
FC=f95nag
can_do_F90=true
MODULES=-mdir $(MODDIR)
EXTRAS	= -f77
DEBUG_FLAGS = -g -C=all -O0
PROF_FLAGS  = -O -pg
PROF_FLAGS  = -pg -O3
PROD_FLAGS  = -O3
REAL_4B = real*4
endif

# Set options for the Fujitsu compiler - on Linux/Intel.
ifeq ($(FORTRAN_COMPILER),FUJITSU)
FC=f95
can_do_F90=true
MODULES=-Am -M$(MODDIR)
EXTRAS  = -ml=cdecl -fw
EXTRAS  = -fw
DEBUG_FLAGS = -g
PROF_FLAGS  = -pg -O3
PROD_FLAGS  = -O -K fast
REAL_4B = real\(4\)
endif

# Set options for the Compaq fort compiler - on alphas.
ifeq ($(FORTRAN_COMPILER),DECFOR)
FC=f95
can_do_F90=true
MODULES=-module $(MODDIR)
EXTRAS	=
DEBUG_FLAGS = -g -arch host -check bounds -check overflow -check nopower -std90
PROF_FLAGS  = -pg -O3
PROD_FLAGS  = -O -fast -inline speed -pipeline
REAL_4B = real\(4\)
endif

DEFINES += -DREAL_4B=$(REAL_4B)

# Set options for the SunOS fortran compiler.
ifeq ($(FORTRAN_COMPILER),SunOS)
FC=f90
can_do_F90=true
MODULES=-M$(MODDIR)
MOVE_MODULES_COMMAND=/usr/bin/mv -f *.mod $(MODDIR)
EXTRAS  =
DEBUG_FLAGS = -C
PROF_FLAGS  = -pg
PROD_FLAGS  = -fast
endif

# Sets options for debug compilation
ifeq ($(compilation),debug)
buildtype = _debug
DEFINES += -DDEBUG $(STATIC)
FLAGS   = $(DEBUG_FLAGS) 
endif

# Sets options for profiling compilation
ifeq ($(compilation),profiling)
buildtype = _prof
DEFINES += -DPROFILING $(STATIC)
FLAGS   = $(PROF_FLAGS) 
endif

# Sets options for production compilation
ifeq ($(compilation),production)
buildtype = _prod
DEFINES += -DPRODUCTION $(STATIC)
FLAGS   = $(PROD_FLAGS) 
endif

# For making the source code documentation.
PROTEX	= protex -b -n -s

.SUFFIXES:
.SUFFIXES: .F90

LINKDIR	= -L$(LIBDIR)

CPPFLAGS	= $(DEFINES) $(INCDIRS)
FFLAGS  	= $(DEFINES) $(FLAGS) $(MODULES) $(INCDIRS) $(EXTRAS)
F90FLAGS  	= $(FFLAGS)
LDFLAGS		+= $(FFLAGS) $(LINKDIR)

#
# Common rules
#
ifeq  ($(can_do_F90),true)
%.o: %.F90
	$(FC) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
else
%.f90: %.F90
	$(CPP) $(CPPFLAGS) $< -o $@

%.o: %.f90
	$(FC) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
endif

#clean:
#	-rm -f ${LIB}

#realclean: clean
#	-rm -f *.o
