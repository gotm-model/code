#$Id: Makefile,v 1.2 2003-03-10 08:33:24 gotm Exp $
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
	$(RM) ${LIB}

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
