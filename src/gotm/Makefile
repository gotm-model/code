#$Id: Makefile,v 1.1 2001-02-12 15:55:59 gotm Exp $
#
# Makefile to build the the main library - libgotm.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libgotm$(buildtype).a

DOCSRC	= \
main.F90		\
gotm.F90

MOD	= \
${LIB}(gotm.o)


OBJ   = \
${LIB}(main.o)

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../doc/gotm.tex 

clean:
	$(RM) -f ${LIB} $(MODDIR)/gotm.{m,mod}

realclean: clean
	$(RM) -f *.o

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2000 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
