#$Id: Makefile,v 1.4 2003-03-12 13:02:01 lars Exp $
#
# Makefile for making the source code documentation. 
#

SHELL	= /bin/sh

SRCDIR = ../src

PROTEXSRC = gotm.tex              \
            meanflow.tex          \
            turbulence.tex        \
            airsea.tex            \
            observations.tex      \
            output.tex            \
            util.tex              \
            sediment.tex          \
            seagrass.tex

TEXSRC    = introduction.tex      \
            mainIntro.tex         \
            meanflowIntro.tex     \
            turbulenceIntro.tex   \
            airseaIntro.tex       \
            observationsIntro.tex \
            outputIntro.tex       \
            utilIntro.tex         \
            extraIntro.tex        \
            cases.tex             \
            gotm.bib


TEXHTMLSRC    = introduction.tex          \
                mainIntro.tex             \
                meanflowIntroHTML.tex     \
                turbulenceIntro.tex       \
                airseaIntro.tex           \
                observationsIntro.tex     \
                outputIntro.tex           \
                utilIntroHTML.tex         \
                extraIntro.tex            \
                cases.tex                 \
                gotm.bib

.PHONY :  clean realclean distclean 


all: code_a4.pdf code_letter.pdf code_html.pdf

$(PROTEXSRC):
	$(MAKE) doc -C $(SRCDIR)

code_a4.tex: $(PROTEXSRC) $(TEXSRC)
	touch $@

code_letter.tex: $(PROTEXSRC)  $(TEXSRC)
	touch $@

code_html.tex: $(PROTEXSRC) $(TEXHTMLSRC)
	$(MAKE) doc -C $(SRCDIR)
	touch $@

code_a4.dvi: code_a4.tex
	latex code_a4.tex
	bibtex code_a4
	latex code_a4.tex
	latex code_a4.tex
	latex code_a4.tex

code_letter.dvi: code_letter.tex
	latex code_letter.tex
	bibtex code_letter
	latex code_letter.tex
	latex code_letter.tex
	latex code_letter.tex

code_html.dvi: code_html.tex
	latex code_html.tex
	bibtex code_html
	latex code_html.tex
	latex code_html.tex
	latex code_html.tex

code_a4.ps: code_a4.dvi
	dvips -o $@ code_a4.dvi

code_letter.ps: code_letter.dvi
	dvips -o $@ code_letter.dvi

code_html.ps: code_html.dvi
	dvips -o $@ code_html.dvi

code_a4.pdf: code_a4.ps
	ps2pdf code_a4.ps

code_letter.pdf: code_letter.ps
	ps2pdf code_letter.ps

code_html.pdf: code_html.ps
	ps2pdf code_html.ps

html:   code_html.tex
	latex2html code_html.tex
	touch html

html_scp: html
	scp -r code_html/* gotm@80.198.12.199:code_html/
	touch html_scp

clean:
	$(RM) *.log *.toc *.aux *.bbl *.blg *.bac *~

realclean: clean
	$(RM) *.dvi *.ps *.pdf
	$(RM) -r code_html/

distclean: realclean
	$(RM)  *.gz *.zip *.Z 

