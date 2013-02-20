#
# Makefile to build the the Input library - libinput.a
#

include ../Rules.make

LIB	= $(LIBDIR)/libinput$(buildtype).a

DOCSRC	= \
input.F90

MOD	= \
${LIB}(input.o)

all: $(MOD)
	$(MOVE_MODULES_COMMAND)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/observations.tex
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/input.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
