#$Id: Makefile,v 1.2 2003-07-23 12:27:31 hb Exp $
#
# Makefile to build the Bio Module - libbio.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libbio$(buildtype).a

DOCSRC	= bio.F90 bio_var.F90 bio_template.F90 bio_npzd.F90 \
          process_model.F90 ode_solvers.F90 bio_save.F90

OBJ   = \
${LIB}(bio_var.o)		\
${LIB}(bio_template.o)		\
${LIB}(bio_npzd.o)		\
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
