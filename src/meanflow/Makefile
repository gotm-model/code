#$Id: Makefile,v 1.6 2004-01-12 08:51:29 lars Exp $
#
# Makefile to build the the Dynamics library - libmeanflow.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libmeanflow$(buildtype).a

DOCSRC	= \
meanflow.F90		\
updategrid.F90		\
adaptivegrid.F90	\
coriolis.F90		\
uequation.F90		\
vequation.F90		\
extpressure.F90		\
intpressure.F90		\
friction.F90		\
temperature.F90		\
salinity.F90		\
buoyancy.F90		\
stratification.F90	\
convectiveadjustment.F90 \
production.F90

MOD	= \
${LIB}(meanflow.o)

OBJ   = \
${LIB}(updategrid.o)		\
${LIB}(adaptivegrid.o)		\
${LIB}(coriolis.o)		\
${LIB}(uequation.o)		\
${LIB}(vequation.o)		\
${LIB}(extpressure.o)		\
${LIB}(intpressure.o)		\
${LIB}(friction.o)		\
${LIB}(temperature.o)		\
${LIB}(salinity.o)		\
${LIB}(stratification.o)	\
${LIB}(buoyancy.o)		\
${LIB}(convectiveadjustment.o)	\
${LIB}(production.o)


all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/meanflow.tex 
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/meanflow.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
