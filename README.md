## Short guide on how to compile GOTM

A more comprehensive guide at the [GOTM official homepage](http://www.gotm.net) (when opdated mid June 2016)

The following prerequisites must be fulfilled before compilation is started

1. The source code for GOTM and [FABM](www.fabm.net) must have been cloned from Git repositories. The actual cloning will depend on the platform and Git-utilities used. Further information is provide [here](https://help.github.com/articles/cloning-a-repository/#platform-linux)
2. A Fortran compiler supporting at least Fortran 2003 must be avaialble
   * On Linux [gfortran](https://gcc.gnu.org/fortran/) versions including and above 4.7 have been tested as well has the [Intel Fortran compiler](https://software.intel.com/en-us/fortran-compilers).
   * On Windows the [Intel Fortran compiler](https://software.intel.com/en-us/fortran-compilers) configured with VisualStudio is working.
3. [NetCDF](http://www.unidata.ucar.edu/software/netcdf)
   * On Linux/Mac GOTM and NetCDF must be compiled with the same Fortran compiler. The configuration and compilation of the NetCDF library is beyond the purpose of this guide.
   * On Windows NetCDF is provided in the repository - compatible with the Intel Fortran compiler
4. [CMake](www.cmake.org) must be installed. CMake is used to configure the compilation and generate native build systems - i.e. Make-based systems on Linux/Mac and VisualStudio on Windows. CMake can be run in command-line and GUI-mode. Further information is provided [here](https://cmake.org/documentation/). A detailed description is beyond the purpose of this guide.

Only when the above 4 points are checked it makes sense to proceed.

#### On Linux/Unix/Mac

Text in *italics* are commands and text in **bold** are variables.

In the following it is assumed the GOTM and FABM source code is cloned to **GOTM_BASE** and **FABM_BASE**. Default values are:
* **GOTM_BASE** = *$HOME/GOTM/code*
* **FABM_BASE** = *$HOME/FABM/code*

##### Configuring using the provided script
CMake advocates *out of source compilation* i.e. the actual compilation is separated from the source code. The first step is to create a *build directory* and change to it:
* *mkdir -p $HOME/build/gotm/ && cd $HOME/build/gotm*

Executing the script *$GOTM_BASE/scripts/linux/gotm_configure.sh* will generate a Make-based build system in a sub-directory named after the compiler used - default is *gfortran*.

This step must be completed without any errors before advancing to the actual compilation.

##### Building using the provided script
Executing the script *$GOTM_BASE/scripts/linux/gotm_build.sh* will compile GOTM according to the configuration carried out in the previous step.

A manual build can also be done like:
* *cd $HOME/build/gotm/<compiler> && make install*

CMake *installs* the generated executable and libraries in an *install_directory* - default i **$HOME/local**. 
To test if the compilation has been succesful - try:
* *$HOME/local/bin/gotm -c*

For furher use of GOTM it is a big advantage to add **$HOME/local/bin** to the **PATH**.

The scripts used above for configuration and compilation have some documentation included and it should be relative easy to adjust to specific need and taste.


#### On Windows
Still to be done
