#
# Makefile for making the source code documentation.
#

SHELL	   = /bin/sh

PROTEX	= protex -b -n -s

SRCDIR    = ../src

PROTEXSRC  = gotm.tex              \
             meanflow.tex          \
             turbulence.tex        \
             airsea.tex            \
             observations.tex      \
             output.tex            \
             util.tex              \
             bio.tex               \
             seagrass.tex          \
             sediment.tex

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

DOCSRC_gotm = $(SRCDIR)/gotm/main.F90          \
              $(SRCDIR)/gotm/gotm.F90          \
              $(SRCDIR)/gotm/print_version.F90
DOCSRC_meanflow = $(SRCDIR)/meanflow/meanflow.F90             \
                  $(SRCDIR)/meanflow/updategrid.F90           \
                  $(SRCDIR)/meanflow/coriolis.F90             \
                  $(SRCDIR)/meanflow/uequation.F90            \
                  $(SRCDIR)/meanflow/vequation.F90            \
                  $(SRCDIR)/meanflow/extpressure.F90          \
                  $(SRCDIR)/meanflow/intpressure.F90          \
                  $(SRCDIR)/meanflow/friction.F90             \
                  $(SRCDIR)/meanflow/temperature.F90          \
                  $(SRCDIR)/meanflow/salinity.F90             \
                  $(SRCDIR)/meanflow/buoyancy.F90             \
                  $(SRCDIR)/meanflow/shear.F90                \
                  $(SRCDIR)/meanflow/stratification.F90       \
                  $(SRCDIR)/meanflow/convectiveadjustment.F90
DOCSRC_turbulence = $(SRCDIR)/turbulence/turbulence.F90       \
                    $(SRCDIR)/turbulence/production.F90       \
                    $(SRCDIR)/turbulence/alpha_mnb.F90        \
                    $(SRCDIR)/turbulence/r_ratio.F90          \
                    $(SRCDIR)/turbulence/tkeeq.F90            \
                    $(SRCDIR)/turbulence/q2over2eq.F90        \
                    $(SRCDIR)/turbulence/kbeq.F90             \
                    $(SRCDIR)/turbulence/lengthscaleeq.F90    \
                    $(SRCDIR)/turbulence/dissipationeq.F90    \
                    $(SRCDIR)/turbulence/genericeq.F90        \
                    $(SRCDIR)/turbulence/tkealgebraic.F90     \
                    $(SRCDIR)/turbulence/kbalgebraic.F90      \
                    $(SRCDIR)/turbulence/algebraiclength.F90  \
                    $(SRCDIR)/turbulence/epsbalgebraic.F90    \
                    $(SRCDIR)/turbulence/variances.F90        \
                    $(SRCDIR)/turbulence/ispralength.F90      \
                    $(SRCDIR)/turbulence/potentialml.F90      \
                    $(SRCDIR)/turbulence/cmue_a.F90           \
                    $(SRCDIR)/turbulence/cmue_b.F90           \
                    $(SRCDIR)/turbulence/cmue_c.F90           \
                    $(SRCDIR)/turbulence/cmue_d.F90           \
                    $(SRCDIR)/turbulence/cmue_ma.F90          \
                    $(SRCDIR)/turbulence/cmue_sg.F90          \
                    $(SRCDIR)/turbulence/cmue_rf.F90          \
                    $(SRCDIR)/turbulence/compute_cpsi3.F90    \
                    $(SRCDIR)/turbulence/compute_rist.F90     \
                    $(SRCDIR)/turbulence/internal_wave.F90    \
                    $(SRCDIR)/turbulence/fk_craig.F90         \
                    $(SRCDIR)/turbulence/kpp.F90              \
                    $(SRCDIR)/turbulence/gotm_lib_version.F90
DOCSRC_airsea = $(SRCDIR)/airsea/airsea.F90               \
                $(SRCDIR)/airsea/airsea_variables.F90     \
                $(SRCDIR)/airsea/humidity.F90             \
                $(SRCDIR)/airsea/back_radiation.F90       \
                $(SRCDIR)/airsea/airsea_fluxes.F90        \
                $(SRCDIR)/airsea/kondo.F90                \
                $(SRCDIR)/airsea/fairall.F90              \
                $(SRCDIR)/airsea/short_wave_radiation.F90 \
                $(SRCDIR)/airsea/albedo_water.F90
DOCSRC_observations = $(SRCDIR)/observations/observations.F90       \
                      $(SRCDIR)/observations/analytical_profile.F90 \
                      $(SRCDIR)/observations/const_NNT.F90          \
                      $(SRCDIR)/observations/const_NNS.F90
DOCSRC_output = $(SRCDIR)/output/output.F90   \
                $(SRCDIR)/output/asciiout.F90 \
                $(SRCDIR)/output/ncdfout.F90
DOCSRC_util = $(SRCDIR)/util/util.F90           \
              $(SRCDIR)/util/diff_center.F90    \
              $(SRCDIR)/util/diff_face.F90      \
              $(SRCDIR)/util/adv_center.F90     \
              $(SRCDIR)/util/lagrange.F90       \
              $(SRCDIR)/util/tridiagonal.F90    \
              $(SRCDIR)/util/eqstate.F90        \
              $(SRCDIR)/util/gridinterpol.F90   \
              $(SRCDIR)/util/convert_fluxes.F90 \
              $(SRCDIR)/util/time.F90           \
              $(SRCDIR)/util/lagrange.F90       \
              $(SRCDIR)/util/ode_solvers.F90
DOCSRC_input = $(SRCDIR)/input/input.F90
DOCSRC_bio = $(SRCDIR)/fabm/gotm_fabm.F90
DOCSRC_seagrass = $(SRCDIR)/extras/seagrass/seagrass.F90
DOCSRC_sediment = $(SRCDIR)/extras/sediment/sediment.F90

.PHONY :     all clean realclean distclean protex \
             a4_dvi letter_dvi html_dvi           \
             a4_ps  letter_ps  html_ps            \
             a4_pdf letter_pdf html_pdf           \
             html_web upload


all:         a4_pdf letter_pdf html_pdf html_web


# do the pdf-versions
a4_pdf:      protex a4.pdf

letter_pdf:  protex letter.pdf

html_pdf:    protex html.pdf

# do the postscript-versions
a4_ps:       protex a4.ps

letter_ps:   protex letter.ps

html_ps:     protex html.ps

# do the dvi-versions
a4_dvi:      protex a4.dvi

letter_dvi:  protex letter.dvi

html_dvi:    protex html.dvi

# do the web-version
html_web:    protex html

protex: $(PROTEXSRC)

gotm.tex: $(DOCSRC_gotm)
	$(PROTEX) $(DOCSRC_gotm) > gotm.tex
meanflow.tex: $(DOCSRC_meanflow)
	$(PROTEX) $(DOCSRC_meanflow) > meanflow.tex
turbulence.tex: $(DOCSRC_turbulence)
	$(PROTEX) $(DOCSRC_turbulence) > turbulence.tex
airsea.tex: $(DOCSRC_airsea)
	$(PROTEX) $(DOCSRC_airsea) > airsea.tex
observations.tex: $(DOCSRC_observations)
	$(PROTEX) $(DOCSRC_observations) > observations.tex
output.tex: $(DOCSRC_output)
	$(PROTEX) $(DOCSRC_output) > output.tex
util.tex: $(DOCSRC_util)
	$(PROTEX) $(DOCSRC_util) > util.tex
input.tex: $(DOCSRC_input)
	$(PROTEX) $(DOCSRC_input) > input.tex
bio.tex: $(DOCSRC_bio)
	$(PROTEX) $(DOCSRC_bio) > bio.tex
seagrass.tex: $(DOCSRC_seagrass)
	$(PROTEX) $(DOCSRC_seagrass) > seagrass.tex
sediment.tex: $(DOCSRC_sediment)
	$(PROTEX) $(DOCSRC_sediment) > sediment.tex

a4.dvi: a4.tex  $(TEXSRC) $(PROTEXSRC) figures
	latex  a4.tex
	bibtex a4
	latex  a4.tex
	latex  a4.tex
	latex  a4.tex

letter.dvi: letter.tex $(TEXSRC) $(PROTEXSRC) figures
	latex  letter.tex
	bibtex letter
	latex  letter.tex
	latex  letter.tex
	latex  letter.tex

html.dvi: html.tex $(TEXSRC) $(PROTEXSRC) figures
	latex  html.tex
	bibtex html
	latex  html.tex
	latex  html.tex
	latex  html.tex

a4.ps: a4.dvi
	dvips -o $@ a4.dvi

letter.ps: letter.dvi
	dvips -o $@ letter.dvi

html.ps: html.dvi
	dvips -o $@ html.dvi

a4.pdf: a4.ps
	ps2pdf a4.ps

letter.pdf: letter.ps
	ps2pdf letter.ps

html.pdf: html.ps
	ps2pdf html.ps

html:   html.dvi
	latex2html -antialias_text -white html.tex
	touch html

upload: a4.pdf letter.pdf html
upload: a4.pdf letter.pdf
	rsync -avz a4.pdf letter.pdf gotm.net:/var/www/www.gotm.net/htdocs/pages/documentation/manual/devel/pdf
	rsync -avz html gotm.net:/var/www/www.gotm.net/htdocs/pages/documentation/manual/devel/

figures:
	wget -nd -nH -q http://www.gotm.net/data_files/gotm-figs-4.0.gz
	tar -xvzf gotm-figs-4.0.gz
	$(RM) gotm-figs-4.0.gz

clean:
	$(RM) *.log *.toc *.aux *.bbl *.blg *.bac *~

realclean: clean
	$(RM) *.dvi *.ps *.pdf
	$(RM) -rf html/
	$(RM) -rf $(PROTEXSRC)

distclean: realclean
	$(RM) *.gz *.zip *.Z
	$(RM) html html_scp pdf_scp
	$(RM) -rf figures

