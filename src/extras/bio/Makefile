#$Id: Makefile,v 1.5 2003-10-28 10:22:45 hb Exp $
#
# Makefile to build the Bio Module - libbio.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libbio$(buildtype).a

DOCSRC	= bio.F90 bio_var.F90 bio_template.F90 bio_npzd.F90 bio_iow.F90 \
          bio_sed.F90 \
          process_model.F90 ode_solvers.F90 bio_save.F90

OBJ   = \
${LIB}(bio_var.o)		\
${LIB}(bio_template.o)		\
${LIB}(bio_npzd.o)		\
${LIB}(bio_iow.o)		\
${LIB}(bio_sed.o)		\
${LIB}(process_model.o)		\
${LIB}(ode_solvers.o)		\
${LIB}(bio_save.o)		\
${LIB}(mussels.o)		\
${LIB}(bio.o)

all: ${OBJ}
	$(MOVE_MODULES_COMMAND)

doc:
	$(PROTEX) $(DOCSRC) > ../../../doc/bio.tex 

clean:
	$(RM) ${LIB}

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2003 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
