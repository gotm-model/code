#$Id: Makefile,v 1.1 2002-04-30 14:22:55 gotm Exp $
#
# Makefile to build the 'lagrange' module - liblagrange.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/liblagrange$(buildtype).a

DOCSRC	= lagrange.F90

OBJ   = \
${LIB}(lagrange.o)

all: ${OBJ}
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../../doc/lagrange.tex 

clean:
	-rm -f ${LIB}

realclean: clean
	-rm -f *.o

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2002 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
