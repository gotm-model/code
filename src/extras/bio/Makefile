#$Id: Makefile,v 1.1 2003-04-01 17:01:00 hb Exp $
#
# Makefile to build the Bio Module - libbio.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libbio$(buildtype).a

DOCSRC	= bio.F90

OBJ   = \
${LIB}(bio.o)

all: ${OBJ}
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../../doc/bio.tex 

clean:
	$(RM) ${LIB}

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2003 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
