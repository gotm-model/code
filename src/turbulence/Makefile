#$Id: Makefile,v 1.5 2004-01-27 10:02:38 hb Exp $
#
# Makefile to build the the Turbulence library - libturbulence.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libturbulence$(buildtype).a


DOCSRC	= \
turbulence.F90		\
tkeeq.F90		\
q2over2eq.F90		\
lengthscaleeq.F90	\
dissipationeq.F90	\
genericeq.F90	        \
tkealgebraic.F90	\
algebraiclength.F90	\
ispralength.F90		\
potentialml.F90	        \
cmue_gen.F90		\
cmue_bb.F90		\
cmue_bbqe.F90		\
cmue_ca.F90		\
cmue_caqe.F90		\
cmue_cb.F90		\
cmue_cbqe.F90		\
cmue_kc.F90		\
cmue_kcqe.F90		\
cmue_my.F90		\
cmue_gpqe.F90		\
cmue_ma.F90		\
cmue_sg.F90		\
cmue_rf.F90		\
fk_craig.F90            \
turbulence_adv.F90      \
gotm_lib_version.F90

MOD   = \
${LIB}(turbulence.o)

OBJ   = \
${LIB}(gotm_lib_version.o)	\
${LIB}(tkeeq.o)			\
${LIB}(q2over2eq.o)		\
${LIB}(tkealgebraic.o)		\
${LIB}(algebraiclength.o)	\
${LIB}(dissipationeq.o)		\
${LIB}(genericeq.o)		\
${LIB}(lengthscaleeq.o)		\
${LIB}(turbulence_adv.o)	\
${LIB}(ispralength.o)		\
${LIB}(potentialml.o)		\
${LIB}(fk_craig.o)		\
${LIB}(cmue_gen.o)		\
${LIB}(cmue_bb.o)		\
${LIB}(cmue_bbqe.o)		\
${LIB}(cmue_ca.o)		\
${LIB}(cmue_caqe.o)		\
${LIB}(cmue_cb.o)		\
${LIB}(cmue_cbqe.o)		\
${LIB}(cmue_kc.o)		\
${LIB}(cmue_my.o)		\
${LIB}(cmue_kcqe.o)		\
${LIB}(cmue_gpqe.o)		\
${LIB}(cmue_ma.o)		\
${LIB}(cmue_rf.o)		\
${LIB}(cmue_sg.o)		\
#${LIB}(internal_wave.o)         

all: $(MOD) ${OBJ}
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/turbulence.tex 
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/turbulence.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
