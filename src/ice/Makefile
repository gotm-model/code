#
# Makefile to build the the ice interaction library - libice.a
#

include ../Rules.make

LIB	= ../../libice$(buildtype).a
LIB	:= $(LIBDIR)/libice$(buildtype).a

DOCSRC	=\
ice.F90			\

MOD	= \
${LIB}(ice_uvic.o)	\
${LIB}(ice_winton.o)	\
${LIB}(ice.o)

OBJ	= 

all: $(MOD) $(OBJ)
	$(MOVE_MODULES_COMMAND)

$(OBJ): $(MOD)

doc:    $(DOCSRC)
	$(PROTEX) $(DOCSRC) > ../../doc/ice.tex
	touch doc

clean:
	$(RM) -f ${LIB} $(MODDIR)/ice.{m,mod} doc

realclean: clean
	$(RM) -f *.o *~

distclean: realclean

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
