This directory contains the Visual Studio 2008 solution and projects for building the GOTM-Python library, used by GOTM-GUI.

This setup requires Visual Studio 2008 to be installed, together with a recent version of Intel Visual Fortran - version 10 or higher should work. Note that the solution is configured for Intel Visual Fortran 11.x. If you have another version, make sure to change the mention of environment variable "IFORT_COMPILER11" to, for instance, "IFORT_COMPILER10" for Intel Visual Fortran 10. This environment variable is used to specify additional library directories for the linker in the C++ project.

The solution uses the following environment variables, which must be set before compilation:

PythonDir - the root directory of your Python installation, for instance "C:\Program Files\Python26"

NETCDF_PREFIX - the path to your NetCDF installation, containing at least "include/netcdf.inc" and "lib/netcdf.lib".

The library is built at "./Debug/gotm.pyd" or "./Release/gotm.pyd", depending on the configuration selected. This library must be copied to the "gui.py" to be used by GOTM-GUI.
