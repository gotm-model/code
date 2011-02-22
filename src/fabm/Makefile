#$Id: Makefile,v 1.1 2011-01-13 12:07:16 jorn Exp $
#
# Makefile to build the FABM-GOTM link module
#

include ../Rules.make

LIB	= $(LIBDIR)/libgotm_fabm$(buildtype).a

DOCSRC	=  gotm_fabm.F90  

OBJ   = ${LIB}(gotm_fabm.o)

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
