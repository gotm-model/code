#
# Makefile to build the the Observtions library - libobservations.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libobservations$(buildtype).a

DOCSRC	= \
observations.F90 	\
analytical_profile.F90	\
const_NNT.F90		\
const_NNS.F90


MOD	= \
${LIB}(observations.o)

OBJ	= \
${LIB}(analytical_profile.o)	\
${LIB}(const_NNT.o)             \
${LIB}(const_NNS.o)

#kbkall: $(MOD)
all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/observations.tex
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/observations.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
