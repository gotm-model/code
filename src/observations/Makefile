#$Id: Makefile,v 1.3 2004-01-12 08:51:42 lars Exp $
#
# Makefile to build the the Observtions library - libobservations.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libobservations$(buildtype).a

DOCSRC	= \
observations.F90 	\
get_s_profile.F90       \
get_t_profile.F90       \
get_ext_pressure.F90    \
get_int_pressure.F90    \
read_extinction.F90     \
get_w_adv.F90           \
get_zeta.F90            \
get_vel_profile.F90     \
get_eps_profile.F90	\
analytical_profile.F90

MOD	= \
${LIB}(observations.o)

OBJ	= \
${LIB}(get_s_profile.o)		\
${LIB}(get_t_profile.o)		\
${LIB}(get_ext_pressure.o)	\
${LIB}(get_int_pressure.o)	\
${LIB}(read_extinction.o)	\
${LIB}(get_w_adv.o)		\
${LIB}(get_zeta.o)		\
${LIB}(get_vel_profile.o)	\
${LIB}(get_eps_profile.o)	\
${LIB}(analytical_profile.o)

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
