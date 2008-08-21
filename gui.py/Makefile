#$Id: Makefile,v 1.8 2008-08-21 14:46:28 jorn Exp $

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
#
# If the NumPy include files and F2Py are not located in the
# default subdirectories under the NumPy root (/core/include
# and /f2py, respectively), you will also need to specify
# these paths in environment variables NUMPYINC and F2PYDIR,
# respectively. This should rarely be necessary.
# -----------------------------------------------------------
# Currently, only compilation with ifort/g++ is supported.
# Compilation with GFORTRAN/g++ has been reported to work;
# however, the Python code will then likely not be able to
# catch Fortran stop statements.
# -----------------------------------------------------------

# If Python and NumPy are installed at default locations, you may get
# away by just setting PYTHONVERSION to your version of Python, e.g.,
# python2.4, python2.5, etc. Other variables such as PYTHONINCDIR,
# PYTHONLIBNAME and NUMPYDIR will then be derived from PYTHONVERSION.
# If this does not work, you will have to set these three variables
# explicitly (see below).
ifndef PYTHONVERSION
PYTHONVERSION = python2.5
endif

# The directory that contains Python header files.
# Below a quick default for our development systems - normal
# users will need to set the PYTHONINCDIR environment variable before
# running make!
ifndef PYTHONINCDIR
PYTHONINCDIR = /usr/include/$(PYTHONVERSION)
endif

# The directory that contains the Python library.
# Below a quick default for our development systems - normal
# users will need to set the PYTHONLIBDIR environment variable before
# running make!
ifndef PYTHONLIBDIR
PYTHONLIBDIR = /usr/lib
endif

# The name of the Python library to link against.
# Below a quick default for our development systems - normal
# users will need to set the PYTHONLIBNAME environment variable before
# running make!
ifndef PYTHONLIBNAME
PYTHONLIBNAME = $(PYTHONVERSION)
endif

# The directory that contains NumPy; it must contain directories
# core/include (NumPy header files) and f2py (Fortran to Python)
# Below some quick defaults for our development systems - normal
# users will need to set the NUMPYDIR environment variable before
# running make!
ifndef NUMPYDIR
NUMPYDIR = /usr/local/lib/$(PYTHONVERSION)/site-packages/numpy
NUMPYDIR = /usr/lib/$(PYTHONVERSION)/site-packages/numpy
endif

# The directory that contains NumPy include files. Normally this can be
# inferred from the NumPy root directory set above (NUMPYDIR). In most
# cases users will therefore not have to set the NUMPYINC environment
# variable. It is possible though.
ifndef NUMPYINC
NUMPYINC = $(NUMPYDIR)/core/include
endif

# The directory that contains F2Py (Fortran-to-Python interface generator.
# Normally this path can be# inferred from the NumPy root directory set
# above (NUMPYDIR). In most cases users will therefore not have to set the
# NUMPYINC environment variable. It is possible though.
ifndef NUMPYINC
F2PYDIR = $(NUMPYDIR)/f2py
endif

# Include the rules for GOTM compilation.
include ../src/Rules.make

# The flag -fexceptions is needed to generate assembly from FORTRAN that is
# capable of passing C++ exceptions (used to catch FORTRAN stop statements).
# The flag -fPIC creates position-independent code, which is good practice
# for shared libraries and required on some architectures (AMD64)
EXTRA_FFLAGS+=-fexceptions -fPIC
CXXFLAGS+=-fPIC

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
#   -L$(PYTHONLIBDIR): directory that contains the Python library
#   -l$(PYTHONLIBNAME): the Python library
#   -lstdc++: the C++ standard library, needed because the Fortran compiler will do the linking.
LDFLAGS += -shared -lgotm$(buildtype) $(ALL_LIBS) -L$(PYTHONLIBDIR) -l$(PYTHONLIBNAME) -lstdc++

.PHONY: clean gotm all

all: gotm gotmmodule.o fortranobject.o gotm-f2pywrappers2.o
	$(FC) ./gotm-f2pywrappers2.o ./gotmmodule.o ./fortranobject.o ./gui_util.o $(LDFLAGS) -o gotm.so

gotm:
	$(MAKE) EXTRA_FFLAGS="$(EXTRA_FFLAGS)" -C ../src all

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

