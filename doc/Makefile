#$Id: Makefile,v 1.8 2004-01-12 08:54:05 lars Exp $
#
# Makefile for making the source code documentation. 
#

SHELL	   = /bin/sh

SRCDIR    = ../src

PROTEXSRC  = gotm.tex              \
             meanflow.tex          \
             turbulence.tex        \
             airsea.tex            \
             observations.tex      \
             output.tex            \
             util.tex              \
             sediment.tex          \
             seagrass.tex          \
             bio.tex

TEXSRC     = introduction.tex      \
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


TEXHTMLSRC = introduction.tex      \
             mainIntro.tex         \
             meanflowIntroHTML.tex \
             turbulenceIntro.tex   \
             airseaIntro.tex       \
             observationsIntro.tex \
             outputIntro.tex       \
             utilIntroHTML.tex     \
             extraIntro.tex        \
             cases.tex             \
             gotm.bib



.PHONY :     all clean realclean distclean protex \
             a4_dvi letter_dvi html_dvi           \
             a4_ps  letter_ps  html_ps            \
             a4_pdf letter_pdf html_pdf           \
             html_web


all:         a4_pdf letter_pdf html_pdf html


# do the pdf-versions
a4_pdf:      protex code_a4.pdf

letter_pdf:  protex code_letter.pdf

html_pdf:    protex code_html.pdf

# do the postscript-versions
a4_ps:       protex code_a4.ps

letter_ps:   protex code_letter.ps

html_ps:     protex code_html.ps

# do the dvi-versions
a4_dvi:      protex code_a4.dvi

letter_dvi:  protex code_letter.dvi

html_dvi:    protex code_html.dvi

# do the web-version
html_web:    protex html


protex:
	$(MAKE) doc -C $(SRCDIR)

code_a4.dvi: code_a4.tex  $(TEXSRC) $(PROTEXSRC) figures
	latex code_a4.tex
	bibtex code_a4
	latex code_a4.tex
	latex code_a4.tex
	latex code_a4.tex

code_letter.dvi: code_letter.tex $(TEXSRC) $(PROTEXSRC) figures
	latex code_letter.tex
	bibtex code_letter
	latex code_letter.tex
	latex code_letter.tex
	latex code_letter.tex

code_html.dvi: code_html.tex $(TEXSRC) $(PROTEXSRC) figures
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

pdf_scp: code_a4.pdf code_letter.pdf
	scp -r code_a4.pdf code_letter.pdf gotm@80.198.12.199:doc/
	touch pdf_scp

html_scp: html
	scp -r code_html/* gotm@80.198.12.199:code_html/
	touch html_scp


figures:
	wget -nd -nH -q http://www.gotm.net/data_files/doc_figures.tar.gz 
	tar -xvzf doc_figures.tar.gz
	$(RM) doc_figures.tar.gz

clean:
	$(RM) *.log *.toc *.aux *.bbl *.blg *.bac *~

realclean: clean
	$(RM) *.dvi *.ps *.pdf
	$(RM) -r code_html/
	$(RM) -r $(PROTEXSRC)

distclean: realclean
	$(RM) *.gz *.zip *.Z
	$(RM) html html_scp pdf_scp
	$(RM) -r figures

