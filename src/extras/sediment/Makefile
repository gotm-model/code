#$Id: Makefile,v 1.2 2003-03-10 08:33:24 gotm Exp $
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
	$(RM) ${LIB}

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2001 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
