#$Id: Rules.make,v 1.34 2011-01-14 09:58:41 jorn Exp $

SHELL   = /bin/sh

# The compilation mode is obtained from $COMPILATION_MODE - 
# default production - else debug or profiling
ifndef COMPILATION_MODE
compilation=production
else
compilation=$(COMPILATION_MODE)
endif

DEFINES=-DNUDGE_VEL
DEFINES=-D$(FORTRAN_COMPILER)

# What do we include in this compilation
NetCDF=false
NetCDF=true
SEDIMENT=false
#SEDIMENT=true
SEAGRASS=false
SEAGRASS=true
ifndef BIO
BIO=true
BIO=false
endif
NO_0D_BIO=false
NO_0D_BIO=true

FEATURES	=
FEATURE_LIBS	=
EXTRA_LIBS	=
INCDIRS		=
LDFLAGS		=

# If we want NetCDF - where are the include files and the library

ifeq ($(NetCDF),true)

DEFINES += -DNETCDF_FMT

ifeq ($(NETCDF_VERSION),NETCDF4)

DEFINES         += -DNETCDF4
INCDIRS         += -I$(shell nf-config --includedir)
NETCDFLIB       =  $(shell nf-config --flibs)

else  # NetCDF3 is default

DEFINES         += -DNETCDF3
ifdef NETCDFINC
INCDIRS         += -I$(NETCDFINC)
endif

ifdef NETCDFLIBDIR
LINKDIRS        += -L$(NETCDFLIBDIR)
endif

ifdef NETCDFLIBNAME
NETCDFLIB       = $(NETCDFLIBNAME)
else
NETCDFLIB       = -lnetcdf
endif

endif

EXTRA_LIBS      += $(NETCDFLIB)

endif
# NetCDF/HDF configuration done

# if we want to include FABM - Framework for Aquatic Biogeochemical Models
ifeq ($(FABM),true)

ifdef FABM_PREFIX

ifeq ($(wildcard $(FABM_PREFIX)/lib/libfabm.*), )
$(error the directory FABM_PREFIX=$(FABM_PREFIX) is not a valid FABM directory)
endif

INCDIRS         += -I$(FABM_PREFIX)/include
LINKDIRS        += -L$(FABM_PREFIX)/lib
EXTRA_LIBS      += -lfabm

else

ifndef FABMDIR
FABMDIR  := $(HOME)/FABM/fabm-git
endif

ifeq ($(wildcard $(FABMDIR)/src/fabm.F90),)
$(error the directory FABMDIR=$(FABMDIR) is not a valid FABM directory)
endif

INCDIRS         += -I$(FABMDIR)/include -I$(FABMDIR)/src/drivers/gotm -I$(FABMDIR)/modules/gotm/$(FORTRAN_COMPILER)
LINKDIRS        += -L$(FABMDIR)/lib/gotm/$(FORTRAN_COMPILER)
EXTRA_LIBS      += -lfabm$(buildtype)

endif

DEFINES += -D_FABM_
FEATURES += fabm
FEATURE_LIBS += -lgotm_fabm$(buildtype)

endif
# FABM configuration done

#
# phony targets
#
.PHONY: clean realclean distclean dummy

# Top of this version of GOTM.
ifndef GOTMDIR
GOTMDIR  := $(HOME)/GOTM/gotm-git
endif
ifeq ($(wildcard $(GOTMDIR)/src/gotm/gotm.F90),)
$(error the directory GOTMDIR=$(GOTMDIR) is not a valid GOTM directory)
endif

CPP	= /lib/cpp

# Here you can put defines for the [c|f]pp - some will also be set depending
# on compilation mode.
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
ifeq ($(NO_0D_BIO),true)
DEFINES         += -DNO_0D_BIO
endif

# Directory related settings.

ifndef BINDIR
BINDIR	= $(GOTMDIR)/bin
endif

ifndef LIBDIR
LIBDIR	= $(GOTMDIR)/lib/$(FORTRAN_COMPILER)
endif

ifndef MODDIR
MODDIR	= $(GOTMDIR)/modules/$(FORTRAN_COMPILER)
endif
INCDIRS	+= -I/usr/local/include -I$(GOTMDIR)/include -I$(MODDIR)

# Normaly this should not be changed - unless you want something very specific.

# The Fortran compiler is determined from the EV FORTRAN_COMPILER - options 
# sofar NAG(linux), FUJITSU(Linux), DECF90 (OSF1 and likely Linux on alpha),
# SunOS, PGF90 - Portland Group Fortran Compiler (on Intel Linux).

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

include $(GOTMDIR)/compilers/compiler.$(FORTRAN_COMPILER)

# For making the source code documentation.
PROTEX	= protex -b -n -s

.SUFFIXES:
.SUFFIXES: .F90

LINKDIRS	+= -L$(LIBDIR)

CPPFLAGS	= $(DEFINES) $(INCDIRS)
FFLAGS  	= $(DEFINES) $(FLAGS) $(MODULES) $(INCDIRS) $(EXTRAS)
F90FLAGS  	= $(FFLAGS)
LDFLAGS		+= $(FFLAGS) $(LINKDIRS)

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
