#
# Makefile for making the source code documentation. 
#

SHELL	= /bin/sh

all: code_a4.dvi code_letter.dvi code_a4.ps code_letter.ps

code_a4.dvi:
	latex code_a4
	latex code_a4
	latex code_a4

code_letter.dvi:
	latex code_letter
	latex code_letter
	latex code_letter

code_a4.ps: code_a4.dvi
	dvips -o $@ code_a4.dvi
	gzip $@

code_letter.ps: code_letter.dvi
	dvips -o $@ code_letter.dvi
	gzip $@

clean:
	-rm -f *.log *.toc *.aux 

realclean: clean
	-rm -f *.dvi *.ps

distclean: realclean
	-rm -f  *.ps.gz

#-----------------------------------------------------------------------
# Copyright (C) 1999 - Hans Burchard and Karsten Bolding (BBH)         !
#-----------------------------------------------------------------------
