#
# Master Makefile for making the GOTM executable.
#

include Rules.make

DOCSRC	= main.F90 gotm.F90

SUBDIRS	= util input observations airsea ice meanflow turbulence output

CORE_LIBS	=	\
		-lice$(buildtype)		\
		-lairsea$(buildtype)		\
		-lmeanflow$(buildtype) 		\
		-lturbulence$(buildtype) 	\
		-lobservations$(buildtype)	\
		-loutput$(buildtype)		\
		-linput$(buildtype)		    \
		-lutil$(buildtype)

ALL_LIBS	= $(FEATURE_LIBS) $(CORE_LIBS) $(EXTRA_LIBS)

EXEC		= gotm$(buildtype)_$(FORTRAN_COMPILER)

all: ../VERSION $(EXEC) install confdir

../VERSION: ../Makefile
	$(MAKE) -C ..

confall: confdir confflags

confdir:
	@echo "GOTMDIR:    "$(GOTMDIR)
	@echo "BINDIR:     "$(BINDIR)
	@echo "MODDIR:     "$(MODDIR)
	@echo "LIBDIR:     "$(LIBDIR)
	@echo "FABMDIR:    "$(FABMDIR)

confflags:
	@echo "DEFINES:    "$(DEFINES)
	@echo "INCDIRS:    "$(INCDIRS)
	@echo "LINKDIRS:   "$(LINKDIRS)
	@echo "EXTRA_LIBS: "$(EXTRA_LIBS)
	@echo "F90FLAGS:   "$(F90FLAGS)



$(EXEC): makedirs subdirs features gotmlib
	$(FC) -o $@ ./gotm/main.o $(LDFLAGS) -lgotm$(buildtype) $(ALL_LIBS)
	$(RM) ./gotm/main.o

makedirs:
	mkdir -p $(MODDIR) $(LIBDIR)

gotmlib:
	$(MAKE) -C gotm all

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

install: $(EXEC)
	mkdir -p $(BINDIR)
	mv $(EXEC) $(BINDIR)

$(SUBDIRS):
	$(MAKE) -C $@

observations airsea meanflow turbulence output input: util

observations airsea: input

features:
ifdef FEATURES
	set -e; for i in $(FEATURES); do $(MAKE) -C $$i; done
endif

doc:
ifdef SUBDIRS
	set -e; for i in gotm $(SUBDIRS); do $(MAKE) -C $$i $@; done
endif
ifdef FEATURES
	set -e; for i in $(FEATURES); do $(MAKE) -C $$i $@; done
endif

bkup: distclean
	tar -czvf ../../backups/bkup.`date +%y%m%d%H`_`cat ../VERSION`.tar.gz *

clean:

realclean: clean
	$(RM) *~
	$(RM) ../modules/$(FORTRAN_COMPILER)/*.{m,mod}
	$(RM) ../lib/$(FORTRAN_COMPILER)/lib*$(buildtype).a
ifdef SUBDIRS
	set -e; for i in gotm $(SUBDIRS); do $(MAKE) -C $$i $@; done
endif
ifdef FEATURES
	set -e; for i in $(FEATURES); do $(MAKE) -C $$i $@; done
endif

distclean: realclean
	$(RM) ../bin/gotm_*

