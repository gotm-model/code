#$Id: Makefile,v 1.3 2003-09-16 12:11:24 hb Exp $
#
# Makefile to build the Bio Module - libbio.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libbio$(buildtype).a

DOCSRC	= bio.F90 bio_var.F90 bio_template.F90 bio_npzd.F90 bio_iow.F90 \
          process_model.F90 ode_solvers.F90 bio_save.F90

OBJ   = \
${LIB}(bio_var.o)		\
${LIB}(bio_template.o)		\
${LIB}(bio_npzd.o)		\
${LIB}(bio_iow.o)		\
${LIB}(process_model.o)		\
${LIB}(ode_solvers.o)		\
${LIB}(bio_save.o)		\
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
