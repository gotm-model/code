#$Id: Makefile,v 1.1 2001-02-12 15:55:57 gotm Exp $
#
# Makefile to build the the airsea interaction library - libairsea.a
#

include ../Rules.make

LIB	= ../../libairsea$(buildtype).a
LIB	:= $(LIBDIR)/libairsea$(buildtype).a

DOCSRC	= \
airsea.F90

MOD	= \
${LIB}(airsea.o)

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

tst_airsea: $(MOD)
	$(FC) -o tst_airsea tst_airsea.F90 $(MOD)

doc:
	$(PROTEX) $(DOCSRC) > ../../doc/airsea.tex 

clean:
	$(RM) -f ${LIB} $(MODDIR)/airsea.{m,mod}

realclean: clean
	$(RM) -f *.o

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
