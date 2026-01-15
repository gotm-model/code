[![Build Status](https://travis-ci.org/gotm-model/code.svg?branch=master)](https://travis-ci.org/gotm-model/code)

## What is GOTM?

GOTM - the **G**eneral **O**cean **T**urbulence **M**odel is an ambitious name for a one-dimensional water column model for marine and limnological applications. It is coupled to a choice of traditional as well as state-of-the-art parameterisations for vertical turbulent mixing. The package consists of the FORTRAN source code, a number of idealised and realistic test cases, and a scientific documentation, all published under the GNU public license.

Further information about GOTM can be found [here](https://gotm.net).

### Manual build and install

GOTM being written in Fortran requires compilation before it can be used.

Below is provided a short description on how to obtain the code, configure, compile and install GOTM.

#### Cloning the code

The [GOTM source code](https://github.com/gotm-model/code) is in a Git repository and as a first step this code most be cloned to the local computer(*). [Git](https://git-scm.com/) must be installed.

Not strictly necessary creating a new folder for GOTM is a good idea.
```
mkdir GOTM
cd GOTM
```
Now clone the code to the local machine:
```
git clone --recurse-submodules https://github.com/gotm-model/code.git
```
#### Configuring the code for compilation

A few requisits must be fullfilled befor proceeding. Being general descriptions the following instructions should work both on Linux/Mac and Windows. The example commands are to be executed in a terminal window - alternatives using a GUI is possible but beyond the instructions given here.

As a common build system we use [CMake](https://cmake.org/) and [Ninja](https://ninja-build.org/). So please install these before proceeding.

Furthermore, a Fortran compiler must be installed and discoverable by CMake.

NetCDF has been - and maybe still will be a headache - but it is assumed that NetCDF is installed with Fortran support. For Windows we provide a prebuilt set of libraries.

Configuration is done with cmake - and example being:
```
cmake -G Ninja -B build -S code
```
This will create a new folder - build - with the necessary information for actual compiling the code.

The build system for GOTM provides support for options to pass to the actual compilation. These are provided as extra arguments to the above cmake execution. As and example to include support for ice - use the following:

```
cmake -B build -S code -DGOTM_USE_STIM
```
It is always safe to completely remove the build  folder and start all over

#### Compiling the code

If the cmake command did not result in any errors we are ready to actually compile the code into an executable:
```
cmake --build build
```
This will take a little while but should en up in a compiled GOTM executable in the build/folder. 
This executable is ready to run but for convinience you might install it in a folder in the PATH on your computer - in which case you can just type - gotm - in any folder.

#### Installing the GOTM executable 
The installation also is done by CMake:
```
cmake --install build --prefix <folder_in_your_path>
```
To test if it works open a new terminal window and execute - gotm.

#### GOTM test cases 
GOTM comes with a number of ready to run test cases. There are also in a Git repository and can be obtained like:
```
git clone --recurse-submodules https://github.com/gotm-model/cases.git
```
Entering any of the cases folders and just executing - gotm - should run the model for the specific configuration.

An older description is provided here [GOTM homepage](http://www.gotm.net/portfolio/software).

(*) For people who intend to contribute to the GOTM source code a 'fork' is a better solution.
