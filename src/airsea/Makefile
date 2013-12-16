#
# Makefile to build the the airsea interaction library - libairsea.a
#

include ../Rules.make

LIB	= ../../libairsea$(buildtype).a
LIB	:= $(LIBDIR)/libairsea$(buildtype).a

LIB1	= ../../libairsea_util$(buildtype).a
LIB1	:= $(LIBDIR)/libairsea_utils$(buildtype).a

DOCSRC	=\
airsea.F90			\
airsea_variables.F90		\
humidity.F90			\
back_radiation.F90		\
airsea_fluxes.F90		\
kondo.F90			\
fairall.F90			\
short_wave_radiation.F90	\
albedo_water.F90

MOD	= \
${LIB1}(airsea_variables.o)	\
${LIB}(airsea.o)

OBJ	= \
${LIB}(short_wave_radiation.o)	\
${LIB}(albedo_water.o)	\
${LIB1}(humidity.o) 		\
${LIB1}(back_radiation.o) 	\
${LIB1}(airsea_fluxes.o) 	\
${LIB1}(kondo.o) 		\
${LIB1}(fairall.o)

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

tst_airsea: $(MOD)
	$(FC) -o tst_airsea tst_airsea.F90 $(MOD)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/airsea.tex
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/airsea.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
