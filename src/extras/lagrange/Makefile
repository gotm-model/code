#$Id: Makefile,v 1.2 2003-03-10 10:15:54 gotm Exp $
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
	$(RM) ${LIB}

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2002 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
