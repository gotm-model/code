#$Id: Makefile,v 1.3 2003-12-11 10:00:09 kbk Exp $
#
# Makefile to build the the main library - libgotm.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libgotm$(buildtype).a

DOCSRC	= \
main.F90  \
gotm.F90

MOD	= \
${LIB}(gotm.o)


OBJ   = \
main.o

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../doc/gotm.tex 

clean:
	$(RM) -f ${LIB} $(MODDIR)/gotm.{m,mod}

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2000 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
