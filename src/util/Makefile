#$Id: Makefile,v 1.6 2004-01-13 08:29:13 lars Exp $
#
# Makefile to build the the Util library - libutil.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libutil$(buildtype).a

DOCSRC	=  diffusionMean.F90 advectionMean.F90 yevol.F90 w_split_it_adv.F90  \
           tridiagonal.F90 eqstate.F90  gridinterpol.F90  time.F90


MOD   = \
${LIB}(time.o)			\
${LIB}(tridiagonal.o)		\
${LIB}(eqstate.o)

OBJ   = \
${LIB}(gridinterpol.o)		\
${LIB}(w_split_it_adv.o)	\
${LIB}(yevol.o)	                \
${LIB}(diffusionMean.o)             \
${LIB}(advectionMean.o)

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

${LIB}(yevol.o): ${LIB}(tridiagonal.o)  ${LIB}(w_split_it_adv.o)

${LIB}(diffusionMean.o): ${LIB}(tridiagonal.o) 

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
