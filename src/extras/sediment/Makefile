#$Id: Makefile,v 1.1 2001-02-12 15:55:57 gotm Exp $
#
# Makefile to build the Sediment Module - libsediment.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libsediment$(buildtype).a

DOCSRC	= sediment.F90

OBJ   = \
${LIB}(sediment.o)

all: ${OBJ}
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../../doc/sediment.tex 

clean:
	-rm -f ${LIB}

realclean: clean
	-rm -f *.o

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
