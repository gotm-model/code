cmake_minimum_required(VERSION 2.8)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Create main project.
project(gotm Fortran)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Specify default build type for single-type systems (not VS)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set (CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Customize compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  set (CMAKE_Fortran_FLAGS "-N 1023")
endif()

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Set default installation prefix.
if(WIN32)
  if(DEFINED ENV{LOCALAPPDATA})
    set(DEFAULT_PREFIX "$ENV{LOCALAPPDATA}/gotm")
  else()
    set(DEFAULT_PREFIX "$ENV{APPDATA}/gotm")
  endif()
else()
  set(DEFAULT_PREFIX "$ENV{HOME}/local/gotm")
endif()

# Global include directories
include_directories("${PROJECT_SOURCE_DIR}/../include"
                    "${PROJECT_SOURCE_DIR}/util"
#KB                    "${CMAKE_BINARY_DIR}/modules"
                   )
# Libraries added in alphabetic order - dependencies are dealt with below 
add_library(airsea OBJECT
            airsea/airsea.F90
            airsea/airsea_fluxes.F90
            airsea/airsea_variables.F90
            airsea/back_radiation.F90
            airsea/fairall.F90
            airsea/humidity.F90
            airsea/kondo.F90
            airsea/short_wave_radiation.F90
           )


add_library(gotm OBJECT
            gotm/gotm.F90
           )

option(USE_ICE "Include ICE support" OFF)
if(USE_ICE)
add_library(ice OBJECT
           )
endif(USE_ICE)

add_library(input OBJECT
            input/input.F90
           )

add_library(meanflow OBJECT
            meanflow/buoyancy.F90
            meanflow/convectiveadjustment.F90
            meanflow/coriolis.F90
            meanflow/extpressure.F90
            meanflow/friction.F90
            meanflow/intpressure.F90
            meanflow/meanflow.F90
            meanflow/salinity.F90
            meanflow/shear.F90
            meanflow/stratification.F90
            meanflow/temperature.F90
            meanflow/uequation.F90
            meanflow/updategrid.F90
            meanflow/vequation.F90
           )

add_library(observations OBJECT
            observations/analytical_profile.F90
            observations/const_NNS.F90
            observations/const_NNT.F90
            observations/observations.F90
           )

add_library(output OBJECT
            output/asciiout.F90
            output/ncdfout.F90
            output/output.F90
           )

add_library(turbulence OBJECT
            turbulence/algebraiclength.F90
            turbulence/alpha_mnb.F90
            turbulence/cmue_a.F90
            turbulence/cmue_b.F90
            turbulence/cmue_c.F90
            turbulence/cmue_d.F90
            turbulence/cmue_ma.F90
            turbulence/cmue_rf.F90
            turbulence/cmue_sg.F90
            turbulence/compute_cpsi3.F90
            turbulence/compute_rist.F90
            turbulence/dissipationeq.F90
            turbulence/epsbalgebraic.F90
            turbulence/fk_craig.F90
            turbulence/genericeq.F90
            turbulence/gotm_lib_version.F90
            turbulence/internal_wave.F90
            turbulence/ispralength.F90
            turbulence/kbalgebraic.F90
            turbulence/kbeq.F90
            turbulence/kpp.F90
            turbulence/lengthscaleeq.F90
            turbulence/potentialml.F90
            turbulence/production.F90
            turbulence/q2over2eq.F90
            turbulence/r_ratio.F90
            turbulence/tkealgebraic.F90
            turbulence/tkeeq.F90
            turbulence/turbulence.F90
            turbulence/algebraiclength.F90
            turbulence/alpha_mnb.F90
            turbulence/cmue_a.F90
            turbulence/cmue_b.F90
            turbulence/cmue_c.F90
            turbulence/cmue_d.F90
            turbulence/cmue_ma.F90
            turbulence/cmue_rf.F90
            turbulence/cmue_sg.F90
            turbulence/compute_cpsi3.F90
            turbulence/compute_rist.F90
            turbulence/dissipationeq.F90
            turbulence/epsbalgebraic.F90
            turbulence/fk_craig.F90
            turbulence/genericeq.F90
            turbulence/gotm_lib_version.F90
            turbulence/internal_wave.F90
            turbulence/ispralength.F90
            turbulence/kbalgebraic.F90
            turbulence/kbeq.F90
            turbulence/kpp.F90
            turbulence/lengthscaleeq.F90
            turbulence/potentialml.F90
            turbulence/production.F90
            turbulence/q2over2eq.F90
            turbulence/r_ratio.F90
            turbulence/tkealgebraic.F90
            turbulence/tkeeq.F90
            turbulence/turbulence.F90
            turbulence/variances.F90
           )

add_library(util OBJECT
            util/adv_center.F90
            util/convert_fluxes.F90
            util/diff_center.F90
            util/diff_face.F90
            util/eqstate.F90
            util/gridinterpol.F90
            util/lagrange.F90
            util/ode_solvers.F90
            util/time.F90
            util/tridiagonal.F90
            util/util.F90
           )

add_dependencies(input util)
add_dependencies(airsea util input)
add_dependencies(gotm meanflow turbulence output)
add_dependencies(output util meanflow turbulence)
add_dependencies(meanflow observations)

option(USE_NetCDF "Include NetCDF support" ON)
if(USE_NetCDF)
  find_package(NetCDF REQUIRED)
  set_property(TARGET output APPEND PROPERTY INCLUDE_DIRECTORIES "${NetCDF_INCLUDE_DIRS}")
  add_definitions(-DNETCDF_FMT -DREAL_4B=real\(4\))
endif(USE_NetCDF)

# For now set to off as default - should maybe be changed
# Use FABM_PREFIX to locate include and lib directories
option(USE_FABM "Include FABM support" ON)
if(USE_FABM)
  find_package(FABM REQUIRED)
  add_library(fabm OBJECT
              fabm/gotm_fabm.F90
              fabm/gotm_fabm_input.F90
              fabm/gotm_fabm_output.F90
             )
  add_dependencies(fabm output)
  if(USE_NetCDF)
    set_property(TARGET fabm APPEND PROPERTY INCLUDE_DIRECTORIES "${NetCDF_INCLUDE_DIRS}")
  endif(USE_NetCDF)
  set_property(TARGET fabm APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  set_property(TARGET gotm APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  add_definitions(-D_FABM_)
  add_dependencies(gotm fabm)
  set(ADDED_SRCS "$<TARGET_OBJECTS:fabm>")
endif(USE_FABM)

# Build GOTM executable.
add_executable(gotm_exe
               gotm/main.F90
               $<TARGET_OBJECTS:gotm>
               $<TARGET_OBJECTS:airsea>
               $<TARGET_OBJECTS:meanflow>
               $<TARGET_OBJECTS:turbulence>
               $<TARGET_OBJECTS:observations>
               $<TARGET_OBJECTS:output>
               $<TARGET_OBJECTS:input>
               $<TARGET_OBJECTS:util>
               ${ADDED_SRCS})
if(USE_FABM)
  target_link_libraries(gotm_exe "${FABM_LIBRARIES}")
endif(USE_FABM)
if(USE_NetCDF)
  target_link_libraries(gotm_exe "${NetCDF_LIBRARIES}")
  if (NetCDF_STATIC_MSVC_BUILD)
    set_property(TARGET gotm_exe PROPERTY LINK_FLAGS_RELEASE "/NODEFAULTLIB:\"libcmtd\"")
    target_compile_options(gotm_exe PRIVATE "/libs:static")
  endif()
endif(USE_NetCDF)

# Link with GOTM libraries.
#KBtarget_link_libraries(gotm airsea meanflow turbulence observations output input util)

# Create a list of institute-specific object libraries.
#KBforeach(institute ${FABM_INSTITUTES}) 
  #KBadd_subdirectory(models/${institute})
  #KBlist(APPEND MODEL_OBJECTS $<TARGET_OBJECTS:${institute}>)
#KBendforeach(institute) 

# Group config source files in a folder in IDEs that support this.
#KBsource_group(config REGULAR_EXPRESSION config/)

# Store FABM *.mod in separate directory, so these files can later be installed.
#KBset_property(TARGET fabm_base fabm PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Add directories with model-specific *.mod to include directories of main FABM library.
#KBforeach(institute ${FABM_INSTITUTES})
  #KBset_property(TARGET fabm APPEND PROPERTY INCLUDE_DIRECTORIES "${CMAKE_BINARY_DIR}/models/${institute}")
#KBendforeach(institute)

# Place projects in specific solution folders (Visual Studio only).
#KBset_property(TARGET fabm_base fabm     PROPERTY FOLDER fabm)
#KBset_property(TARGET ${FABM_INSTITUTES} PROPERTY FOLDER fabm/models)

# Re-initialize installation prefix if needed.
mark_as_advanced(GOTM_DEFAULT_PREFIX)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # Installation prefix is still set to cmake default - overwrite it.
  set(GOTM_INITIALIZE_PREFIX ON)
else()
  # Check whether cached actual and default prefixes match.
  # If so, we'll overwrite prefix with our current preferred (host-specific) default.
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in")
  set(GOTM_DEFAULT_PREFIX  ${DEFAULT_PREFIX} CACHE PATH "Default directory to install GOTM in")
  if (CMAKE_INSTALL_PREFIX STREQUAL GOTM_DEFAULT_PREFIX)
    set(GOTM_INITIALIZE_PREFIX ON)
  endif()
endif()
if (GOTM_INITIALIZE_PREFIX)
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in" FORCE)
  set(GOTM_DEFAULT_PREFIX  ${DEFAULT_PREFIX} CACHE PATH "Default directory to install GOTM in" FORCE)
endif()
message(STATUS "Installation prefix: ${CMAKE_INSTALL_PREFIX}")

# Install GOTM library and include files, unless the host overruled this.
#KBif (NOT DEFINED GOTM_NO_LIBRARY_INSTALL)
  #KBinstall(TARGETS gotm
    #KBDESTINATION lib)
  #KBinstall(DIRECTORY ${CMAKE_BINARY_DIR}/modules/\${BUILD_TYPE}/
    #KBDESTINATION include)
  #KBinstall(FILES ${PROJECT_SOURCE_DIR}/../include/fabm.h ${PROJECT_SOURCE_DIR}/drivers/${FABMHOST}/fabm_driver.h
    #KBDESTINATION include)
#KBendif()

