cmake_minimum_required(VERSION 3.0)

if(NOT PROJECT_NAME STREQUAL gotm)
   project(gotm VERSION 5.4.0 LANGUAGES Fortran)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_LIST_DIR}/cmake/Modules/")

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
   set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Specify default build type for single-build-type systems (not VS)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
   set (CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
   set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# ----------------------------
# User-configurable options
# ----------------------------

option(GOTM_USE_CVMIX "Include support for the CVMix turbulence closure models (github.com/CVMix)" ON)

option(GOTM_USE_FABM "Include support for Framework for Aquatic Biogeochemical Models (fabm.net)" ON)

option(GOTM_USE_STIM "Include support for Simple Thermodynamic Ice Models - STIM" OFF)

option(GOTM_USE_NetCDF "Enable output in NetCDF format" ON)

if(MSVC)
  option(GOTM_EMBED_VERSION "Embed GOTM version information" OFF)
else(MSVC)
  option(GOTM_EMBED_VERSION "Embed GOTM version information" ON)
endif(MSVC)

option(GOTM_USE_SEAGRASS "Enable seagrass module" OFF)

option(GOTM_EXTRA_OUTPUT "Include additional turbulence diagnostics in output" OFF)
mark_as_advanced(GOTM_EXTRA_OUTPUT)

# ----------------------------
# Dependencies
# ----------------------------

if (NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/extern/flexout/src/CMakeLists.txt")
   message(FATAL_ERROR "Flexible output manager not found at extern/flexout. Please retrieve this submodule first by running \"git submodule update --init --recursive\" within your GOTM source directory.")
else()
   set(FLEXOUT_USE_NetCDF ${GOTM_USE_NetCDF} CACHE BOOL "Enable output in NetCDF format" FORCE)
   mark_as_advanced(FLEXOUT_USE_NetCDF)
   add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/extern/flexout" extern/flexout)
endif()

if(GOTM_USE_CVMIX)
   if (NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/extern/CVMix-src/CMakeLists.txt")
      message(FATAL_ERROR "CVMix not found at extern/CVMix-src. Please retrieve this submodule first by running \"git submodule update --init\" within your GOTM source directory - followed by \"git pull --recurse-submodules\".")
   else()
#     mark_as_advanced(FORCE CVMIX_BASE CVMIX_EMBED_VERSION)
      add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/extern/CVMix-src" extern/CVMix-src)
   endif()
endif(GOTM_USE_CVMIX)

if(GOTM_USE_FABM)
   mark_as_advanced(FORCE FABM_PREFIX FABM_BASE FABM_EMBED_VERSION)
   if (NOT FABM_BASE AND FABM_PREFIX)
      # FABM must be pre-built: use FABM_PREFIX to locate existing include and lib directories.
      find_package(fabm REQUIRED CONFIG HINTS "${FABM_PREFIX}" NO_DEFAULT_PATH)
      mark_as_advanced(CLEAR FABM_PREFIX)
   else()
      find_path(FABM_BASE src/fabm.F90 HINTS ${CMAKE_CURRENT_LIST_DIR}/extern/fabm DOC "Path to FABM source directory.")
      set(FABM_EMBED_VERSION ${GOTM_EMBED_VERSION} CACHE BOOL "Embed FABM version information" FORCE)
      set(FABM_FORCED_HOST gotm)
      add_subdirectory("${FABM_BASE}" extern/fabm)
      mark_as_advanced(CLEAR FABM_BASE)
   endif()
endif(GOTM_USE_FABM)

if(GOTM_USE_STIM)
   mark_as_advanced(FORCE STIM_BASE STIM_EMBED_VERSION)
   find_path(STIM_BASE src/stim_version.F90.in HINTS ${CMAKE_CURRENT_LIST_DIR}/extern/stim DOC "Path to STIM source directory.")
   set(STIM_EMBED_VERSION ${GOTM_EMBED_VERSION} CACHE BOOL "Embed STIM version information" FORCE)
   add_subdirectory("${STIM_BASE}" extern/stim)
   mark_as_advanced(CLEAR STIM_BASE)
endif(GOTM_USE_STIM)

# ----------------------------
# GOTM itself
# ----------------------------

# Set default installation prefix (done after dependencies to ensure ours takes precedence)
if(WIN32)
  if(DEFINED ENV{LOCALAPPDATA})
    set(DEFAULT_PREFIX "$ENV{LOCALAPPDATA}/gotm")
  else()
    set(DEFAULT_PREFIX "$ENV{APPDATA}/gotm")
  endif()
else()
  set(DEFAULT_PREFIX "$ENV{HOME}/local/gotm")
endif()
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # Still on CMake default - override with our own default.
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in" FORCE)
else()
  # Just set the doc string for the variable.
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/src/CMakeLists.txt")
add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/tests" tests)

include(FeatureSummary)
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
   #feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES PACKAGES_FOUND)
   feature_summary(FILENAME ${CMAKE_CURRENT_BINARY_DIR}/features.log WHAT ALL)
endif()
