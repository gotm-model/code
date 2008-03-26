#$Id: Makefile,v 1.19 2008-03-26 08:56:52 kb Exp $
#
# Makefile to build the Bio Module - libbio.a
#

include ../../Rules.make

LIB	= $(LIBDIR)/libbio$(buildtype).a

DOCSRC	=  bio.F90 \
           bio_var.F90 \
           bio_fluxes.F90 \
           bio_save.F90

OBJ1   = \
${LIB}(bio_var.o)		\
${LIB}(bio_fluxes.o)		\
${LIB}(bio_save.o)

OBJ2   = \
${LIB}(bio.o)

ifneq ($(BIO_TEMPLATE),false)
SUBDIRS += template
DEFINES += -DBIO_TEMPLATE
endif

ifneq ($(BIO_NPZD),false)
SUBDIRS += npzd
DEFINES += -DBIO_NPZD
endif

ifneq ($(BIO_IOW),false)
SUBDIRS += iow
DEFINES += -DBIO_IOW
endif

ifneq ($(BIO_SED),false)
SUBDIRS += sed
DEFINES += -DBIO_SED
endif

ifneq ($(BIO_FASHAM),false)
SUBDIRS += fasham
DEFINES += -DBIO_FASHAM
endif

ifneq ($(BIO_MAB),false)
SUBDIRS += mab
DEFINES += -DBIO_MAB
endif

ifneq ($(BIO_ROLM),false)
SUBDIRS += rolm
DEFINES += -DBIO_ROLM
endif

ifneq ($(BIO_NPZD_FE),false)
SUBDIRS += npzd_fe
DEFINES += -DBIO_NPZD_FE
endif

all: configure $(OBJ1) OBJS $(OBJ2)
	$(MOVE_MODULES_COMMAND)

configure: BIO_MODELS.sh
	./BIO_MODELS.sh
	touch bio.F90

OBJS: $(SUBDIRS)
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i ; done

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../../doc/bio.tex 
	touch doc

clean:
	$(RM) ${LIB} doc

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2008 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
