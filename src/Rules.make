#$Id: Rules.make,v 1.10 2003-04-04 14:25:51 hb Exp $

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
BIO=false
BIO=true

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
ifndef GOTMDIR
GOTMDIR  := $(HOME)/gotm-3.1.x
endif
GOTMDIR  := $(HOME)/gotm-3.1.x

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
ifeq ($(BIO),true)
DEFINES += -DBIO
FEATURES += extras/bio
FEATURE_LIBS += -lbio$(buildtype)
endif

# Directory related settings.

ifndef BINDIR
BINDIR	= $(GOTMDIR)/bin
endif

ifndef LIBDIR
LIBDIR	= $(GOTMDIR)/lib/$(FORTRAN_COMPILER)
endif

ifndef MODDIR
MODDIR	= $(GOTMDIR)/modules
MODDIR	= $(GOTMDIR)/modules/$(FORTRAN_COMPILER)
endif
INCDIRS	+= -I/usr/local/include -I$(GOTMDIR)/include -I$(MODDIR)

# Normaly this should not be changed - unless you want something very specific.

# The Fortran compiler is determined from the EV FORTRAN_COMPILER - options 
# sofar NAG(linux), FUJITSU(Linux), DECF90 (OSF1 and likely Linux on alpha),
# SunOS, PGF90 - Portland Group Fortran Compiler (on Intel Linux).

include $(GOTMDIR)/compilers/compiler.$(FORTRAN_COMPILER)

DEFINES += -DREAL_4B=$(REAL_4B)

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
#	$(CPP) $(CPPFLAGS) $< -o $@
	$(F90_to_f90)
%.o: %.f90
	$(FC) $(F90FLAGS) $(EXTRA_FFLAGS) -c $< -o $@
endif
