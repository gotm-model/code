#$Id: Makefile,v 1.3 2003-03-10 08:33:23 gotm Exp $
#
# Makefile to build the the Util library - libutil.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libutil$(buildtype).a

DOCSRC	= time.F90 tridiagonal.F90 eqstate.F90 yevol.F90 w_split_it_adv.F90 gridinterpol.F90  


OBJ   = \
${LIB}(time.o)			\
${LIB}(tridiagonal.o)		\
${LIB}(eqstate.o)		\
${LIB}(gridinterpol.o)		\
${LIB}(w_split_it_adv.o)	\
${LIB}(yevol.o)

all: $(OBJ)
	$(MOVE_MODULES_COMMAND)

${LIB}(yevol.o): ${LIB}(tridiagonal.o)  ${LIB}(w_split_it_adv.o)

doc:
	$(PROTEX) $(DOCSRC) > ../../doc/util.tex 

clean:
	$(RM) -f ${LIB} $(MODDIR)/{eqstate,time,tridiagonal}.{m,mod}

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#kbkinclude ../Rules.make

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
