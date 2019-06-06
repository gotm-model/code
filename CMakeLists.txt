cmake_minimum_required(VERSION 3.0)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Create main project.
project(gotm VERSION 5.3.0 LANGUAGES Fortran)

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

option(GOTM_USE_FABM "Include support for Framework for Aquatic Biogeochemical Models (fabm.net)" ON)

option(GOTM_USE_NetCDF "Enable output in NetCDF format" ON)

if (NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/extern/flexout/src/CMakeLists.txt")
   message(FATAL_ERROR "Flexible output manager not found at extern/flexout. Please retrieve this submodule first by running \"git submodule update --init\" within your GOTM source directory.")
else()
   set(FLEXOUT_USE_NetCDF ${GOTM_USE_NetCDF} CACHE BOOL "Enable output in NetCDF format" FORCE)
   mark_as_advanced(FLEXOUT_USE_NetCDF)
   add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/extern/flexout" 
                    "${CMAKE_CURRENT_BINARY_DIR}/extern/flexout"
   )
endif()

if(GOTM_USE_FABM)
   mark_as_advanced(FORCE FABM_PREFIX FABM_BASE)
   if (NOT FABM_BASE AND FABM_PREFIX)
      # FABM must be pre-built: use FABM_PREFIX to locate existing include and lib directories.
      find_package(FABM REQUIRED CONFIG HINTS "${FABM_PREFIX}" NO_DEFAULT_PATH)
      mark_as_advanced(CLEAR FABM_PREFIX)
   else()
      find_path(FABM_BASE src/fabm.F90 HINTS ${CMAKE_CURRENT_SOURCE_DIR}/extern/fabm DOC "Path to FABM source directory.")
      set(FABM_EMBED_VERSION ${GOTM_EMBED_VERSION} CACHE BOOL "Embed FABM version information" FORCE)
      set(FABM_FORCED_HOST gotm)
      add_subdirectory("${FABM_BASE}/src" "${CMAKE_CURRENT_BINARY_DIR}/extern/fabm")
      mark_as_advanced(CLEAR FABM_BASE)
   endif()
endif(GOTM_USE_FABM)

add_subdirectory(src)
add_subdirectory(tests)

include(FeatureSummary)
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
   feature_summary(WHAT ENABLED_FEATURES DISABLED_FEATURES PACKAGES_FOUND)
   feature_summary(FILENAME ${CMAKE_CURRENT_BINARY_DIR}/features.log WHAT ALL)
endif()

install(EXPORT gotmConfig DESTINATION cmake)

