#$Id: Makefile,v 1.1 2001-02-12 15:55:57 gotm Exp $
#
# Makefile to build the Seagrass Module - libseagrass.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libseagrass$(buildtype).a

DOCSRC	= seagrass.F90

OBJ   = \
${LIB}(seagrass.o)

all: ${OBJ}
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../../doc/seagrass.tex 

clean:
	-rm -f ${LIB}

realclean: clean
	-rm -f *.o

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
