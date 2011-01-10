#$Id: Makefile,v 1.1 2011-01-10 12:24:27 jorn Exp $
#
# Makefile to build the RMBM-GOTM link module
#

include ../Rules.make

LIB	= $(LIBDIR)/libgotm_rmbm$(buildtype).a

DOCSRC	=  gotm_rmbm.F90  

OBJ   = ${LIB}(gotm_rmbm.o)

all: $(OBJ)
	$(MOVE_MODULES_COMMAND)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/bio.tex 
	touch doc

clean:
	$(RM) ${LIB} doc

realclean: clean
	$(RM) *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 2011 - Jorn Bruggeman (BBA)                            !
#-----------------------------------------------------------------------
