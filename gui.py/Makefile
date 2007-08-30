#$Id: Makefile,v 1.3 2007-08-30 08:59:40 jorn Exp $

# -----------------------------------------------------------
# Makefile for creating the Python-based GUI for GOTM
# -----------------------------------------------------------
# In addition to the environment variables needed for GOTM
# compilation (NETCDFHOME, NETCDFINC, NETCDFLIBNAME, GOTMDIR
# FORTRAN_COMPILER), this makefile also needs to know about
# the location of Python header files, the location of the
# Python library, the name of the Python library, and the
# location of NumPy. These should be set through variables
# PYTHONINCDIR, PYTHONLIBDIR, PYTHONLIBNAME and NUMPYDIR,
# respectively. If any of these is missing, a default value
# is used that works at least on some systems.
# -----------------------------------------------------------
# Currently, only compilation with ifort/g++ is supported.
# -----------------------------------------------------------

# The directory that contains Python header files.
ifndef PYTHONINCDIR
PYTHONINCDIR = /usr/include/python2.4
endif

# The directory that contains the Python library.
ifndef PYTHONLIBDIR
PYTHONLIBDIR = /usr/lib
endif

# The name of the Python library to link against.
ifndef PYTHONLIBNAME
PYTHONLIBNAME = python2.4
endif

# The directory that contains NumPy; it must contain directories
# core/include (NumPy header files) and f2py (Fortran to Python)
ifndef NUMPYDIR
NUMPYDIR = /usr/local/lib/python2.4/site-packages/numpy
NUMPYDIR = /usr/lib/python2.4/site-packages/numpy
endif

NUMPYINC = $(NUMPYDIR)/core/include
F2PYDIR = $(NUMPYDIR)/f2py

# Include the rules for GOTM compilation.
include ../src/Rules.make

# The flag -fexceptions is needed to generate FORTRAN code capable of 
# passing C++ exceptions
EXTRA_FFLAGS+=-fexceptions

CORE_LIBS	=	\
		-lairsea$(buildtype)		\
		-lmeanflow$(buildtype) 		\
		-lturbulence$(buildtype) 	\
		-lobservations$(buildtype)	\
		-loutput$(buildtype)		\
		-lutil$(buildtype)

ALL_LIBS	= $(FEATURE_LIBS) $(CORE_LIBS) $(EXTRA_LIBS)

# Extra include directories for the C++ code (Python, NumPy and F2PY)
CPPFLAGS += -I$(PYTHONINCDIR) -I$(F2PYDIR)/src -I$(NUMPYINC)

# Extra linker options for our Python-GOTM library. Respectively:
#   -shared: for building a shared library (*.so) rather than an executable
#   -lgotm$(buildtype): the GOTM library built from gotm.F90
#   $(ALL_LIBS): libaries built from GOTM modules (defined above)
#   -l$(PYTHONLIBNAME): the Python library
#   -L$(PYTHONLIB): directory that contains the Python library
#   -lstdc++: the C++ standard library, needed because the Fortran compiler will do the linking.
LDFLAGS += -shared -lgotm$(buildtype) $(ALL_LIBS) -l$(PYTHONLIBNAME) -L$(PYTHONLIBDIR) -lstdc++

.PHONY: clean gotm all

all: gotm gotmmodule.o fortranobject.o gotm-f2pywrappers2.o
	$(FC) ./gotm-f2pywrappers2.o ./gotmmodule.o ./fortranobject.o ./gui_util.o $(LDFLAGS) -o gotm.so

gotm:
	$(MAKE) EXTRA_FFLAGS=$(EXTRA_FFLAGS) -C ../src all

gotmmodule.o:
	$(CXX) -c gotmmodule.cpp $(CXXFLAGS) $(CPPFLAGS)

fortranobject.o:
	$(CXX) -c $(F2PYDIR)/src/fortranobject.c $(CXXFLAGS) $(CPPFLAGS)

gotm-f2pywrappers2.o: gui_util.o

clean: 
	$(MAKE) -C ../src $@
	$(RM) *~ *.o

realclean: clean 
	$(MAKE) -C ../src $@
	$(RM) *.pyc

distclean: realclean 
	$(MAKE) -C ../src $@
	$(RM) *.so

