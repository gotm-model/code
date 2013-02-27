#
# Makefile to build the the Observtions library - libobservations.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libobservations$(buildtype).a

DOCSRC	= \
<<<<<<< HEAD
inflows.F90             \
observations.F90        \
get_s_profile.F90       \
get_t_profile.F90       \
get_o2_profile.F90      \
get_ext_pressure.F90    \
get_int_pressure.F90    \
read_extinction.F90     \
get_w_adv.F90           \
get_zeta.F90            \
get_wave.F90            \
get_vel_profile.F90     \
get_eps_profile.F90	\
=======
observations.F90 	\
>>>>>>> 644454bb0055e73c030425f864d770ac07a8fc43
analytical_profile.F90	\
const_NNT.F90		\
const_NNS.F90


MOD	= \
${LIB}(inflows.o) \
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
