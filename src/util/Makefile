#$Id: Makefile,v 1.5 2004-01-12 08:53:03 lars Exp $
#
# Makefile to build the the Util library - libutil.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libutil$(buildtype).a

DOCSRC	=  diffusion.F90 advection.F90 yevol.F90 w_split_it_adv.F90  \
           tridiagonal.F90 eqstate.F90  gridinterpol.F90  time.F90


MOD   = \
${LIB}(time.o)			\
${LIB}(tridiagonal.o)		\
${LIB}(eqstate.o)

OBJ   = \
${LIB}(gridinterpol.o)		\
${LIB}(w_split_it_adv.o)	\
${LIB}(yevol.o)	                \
${LIB}(diffusion.o)             \
${LIB}(advection.o)

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

${LIB}(yevol.o): ${LIB}(tridiagonal.o)  ${LIB}(w_split_it_adv.o)

${LIB}(diffusion.o): ${LIB}(tridiagonal.o) 

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/util.tex 
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/{eqstate,time,tridiagonal}.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#kbkinclude ../Rules.make

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
