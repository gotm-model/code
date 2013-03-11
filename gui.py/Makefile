# -----------------------------------------------------------
# Makefile for creating the Python-based GUI for GOTM
# -----------------------------------------------------------
# In addition to the environment variables needed for GOTM
# compilation (NETCDFHOME, NETCDFINC, NETCDFLIBNAME, GOTMDIR,
# FORTRAN_COMPILER), this makefile also needs to know about
# the location of Python header files, the location of the
# Python library, the name of the Python library, and the
# location of NumPy. The makefile tries to infer these
# variables by calling "python". If the GUI will be used by
# a python version different from the system "python", this
# must be specified by setting environment variable PYTHON to
# the desired Python interpreter (e.,g., python2.7) before
# calling make. Alternatively, make can be called with
# PYTHON=interpreter as additional argument.
# -----------------------------------------------------------
# Currently, only compilation with ifort/g++ is supported.
# Compilation with GFORTRAN/g++ has been reported to work;
# however, the Python code will then likely not be able to
# catch Fortran stop statements.
# -----------------------------------------------------------

# The name of the Python interpreter. This is called by make
# in order to infer Python-specific paths.
ifndef PYTHON
PYTHON = python
endif

# The directory that contains Python header files.
ifndef PYTHONINCDIR
PYTHONINCDIR = $(shell $(PYTHON) -c "import distutils.sysconfig; print distutils.sysconfig.get_python_inc()")
ifeq ($(PYTHONINCDIR),)
$(error "Unable to auto-detect Python include directory by calling $(PYTHON).")
endif
endif

# The directory that contains the Python library.
ifndef PYTHONLIBDIR
PYTHONLIBDIR = $(shell $(PYTHON) -c "import distutils.sysconfig; print distutils.sysconfig.get_python_lib()")
ifeq ($(PYTHONLIBDIR),)
$(error "Unable to auto-detect Python library directory by calling $(PYTHON).")
endif
endif

# The name of the Python library to link against.
ifndef PYTHONLIBNAME
PYTHONLIBNAME = $(shell $(PYTHON) -c "import sys; print 'python'+sys.version[:3]")
ifeq ($(PYTHONLIBNAME),)
$(error "Unable to auto-detect Python library name by calling $(PYTHON).")
endif
endif

# The directory that contains NumPy include files.
ifndef NUMPYINC
NUMPYINC = $(shell $(PYTHON) -c "import numpy; print numpy.get_include()")
ifeq ($(NUMPYINC),)
$(error "Unable to auto-detect Numpy include directory by calling $(PYTHON).")
endif
endif

# The directory that contains F2Py (Fortran-to-Python interface generator.
ifndef F2PYDIR
F2PYDIR = $(shell $(PYTHON) -c "import numpy.f2py,os.path; print os.path.dirname(numpy.f2py.__file__)")
ifeq ($(F2PYDIR),)
$(error "Unable to auto-detect F2py directory by calling $(PYTHON).")
endif
endif

# Include the rules for GOTM compilation.
export BIO=true
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
		-linput$(buildtype)			\
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

.PHONY: confall clean gotm all distclean realclean

all: gotm gotmmodule.o fortranobject.o gotm-f2pywrappers2.o confall
	$(FC) ./gotm-f2pywrappers2.o ./gotmmodule.o ./fortranobject.o ./gui_util.o $(LDFLAGS) -o gotm.so -Xlinker -zmuldefs

confall:
	@echo "PYTHON:        "$(PYTHON)
	@echo "PYTHONINCDIR:  "$(PYTHONINCDIR)
	@echo "PYTHONLIBDIR:  "$(PYTHONLIBDIR)
	@echo "PYTHONLIBNAME: "$(PYTHONLIBNAME)
	@echo "NUMPYINC:      "$(NUMPYINC)
	@echo "F2PYDIR:       "$(F2PYDIR)

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

