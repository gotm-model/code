#$Id: Makefile,v 1.1 2008-11-05 12:39:26 jorn Exp $

include ../../../Rules.make

EXEC = driver_0d$(buildtype)_$(FORTRAN_COMPILER)

SUBDIRS  = \
	../../../airsea		\
	../../../util		\
	..

all: $(EXEC)

$(EXEC): $(SUBDIRS)
	$(FC) -o $@ ./driver_0d.o ./main.o $(LDFLAGS)

$(SUBDIRS):
	$(MAKE) -C $@

#-----------------------------------------------------------------------
# Copyright (C) 2008 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
