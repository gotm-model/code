#$Id: Makefile,v 1.1 2001-02-12 15:55:57 gotm Exp $
#
# Makefile to build the the Util library - libutil.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libutil$(buildtype).a

DOCSRC	= time.F90 tridiagonal.F90 advection.F90 eqstate.F90 gridinterpol.F90  yevol.F90


OBJ   = \
${LIB}(time.o)			\
${LIB}(tridiagonal.o)		\
${LIB}(advection.o)		\
${LIB}(eqstate.o)		\
${LIB}(gridinterpol.o)		\
${LIB}(yevol.o)

all: $(OBJ)
	$(MOVE_MODULES_COMMAND)

${LIB}(yevol.o): ${LIB}(tridiagonal.o) ${LIB}(advection.o)

doc:
	$(PROTEX) $(DOCSRC) > ../../doc/util.tex 

clean:
	$(RM) -f ${LIB} $(MODDIR)/{eqstate,time,tridiagonal}.{m,mod}

realclean: clean
	$(RM) -f *.o

distclean: realclean

#kbkinclude ../Rules.make

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
