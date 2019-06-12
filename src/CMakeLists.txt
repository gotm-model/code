cmake_minimum_required(VERSION 3.0)

if(NOT TARGET field_manager)
   message(WARNING "You are using ${CMAKE_CURRENT_LIST_DIR} as source directory. This is deprecated. Please use its parent directory instead.")
   include("${CMAKE_CURRENT_LIST_DIR}/../CMakeLists.txt")
   return()
endif()

# ----------------------------
# User-configurable options
# ----------------------------

if(MSVC)
  option(GOTM_EMBED_VERSION "Embed GOTM version information" OFF)
else(MSVC)
  option(GOTM_EMBED_VERSION "Embed GOTM version information" ON)
endif(MSVC)

option(GOTM_USE_SEAGRASS "Enable seagrass module" OFF)

option(GOTM_EXTRA_OUTPUT "Include additional turbulence diagnostics in output" OFF)
mark_as_advanced(GOTM_EXTRA_OUTPUT)

# ----------------------------
# Directory wide settings
# ----------------------------

# Common directory for Fortran modules
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  add_compile_options(-ffree-line-length-none)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  add_compile_options(-N 1023)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  if(WIN32)
    add_compile_options("$<$<CONFIG:DEBUG>:/Od>")
  endif()
endif()

# Include directories
include_directories("${CMAKE_CURRENT_LIST_DIR}/../include")

# Preprocessor definitions
add_definitions(-DFORTRAN95)
if(GOTM_USE_NetCDF)
   add_definitions(-DNETCDF_FMT)
endif()
if(GOTM_USE_FABM)
  add_definitions(-D_FABM_)
endif()
if(GOTM_USE_SEAGRASS)
   add_definitions(-DSEAGRASS)
endif()
if(GOTM_EXTRA_OUTPUT)
  add_definitions(-DEXTRA_OUTPUT)
endif()

# ----------------------------
# Targets
# ----------------------------

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/util util)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/turbulence turbulence)

if(GOTM_USE_FABM)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/fabm fabm)
endif(GOTM_USE_FABM)

if(GOTM_BUILD_LIBRARIES_ONLY)
   return()
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/input input)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/airsea airsea)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/observations observations)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/meanflow meanflow)

if(GOTM_USE_SEAGRASS)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/extras/seagrass seagrass)
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/gotm gotm)

set_property(TARGET gotm input airsea observations meanflow PROPERTY FOLDER gotm)

# All libraries configured

# Build GOTM executable.
add_executable(gotm_exe
   ${CMAKE_CURRENT_LIST_DIR}/gotm/main.F90
)
set_property(TARGET gotm_exe PROPERTY RUNTIME_OUTPUT_NAME "gotm")
target_link_libraries(gotm_exe PRIVATE gotm)

# Installation
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
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # Still on CMake default - override with our own default.
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in" FORCE)
else()
  # Just set the doc string for the variable.
  set(CMAKE_INSTALL_PREFIX ${DEFAULT_PREFIX} CACHE PATH "Directory to install GOTM in")
endif()

# Install GOTM library and include files, unless the host overruled this.
install(TARGETS gotm_exe RUNTIME DESTINATION bin)
install(FILES 
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/turbulence.mod
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/time.mod
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/util.mod
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/gotm_version.mod
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/gotm_compilation.mod
   ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/mtridiagonal.mod
   DESTINATION include
)
install(EXPORT gotmConfig DESTINATION cmake NAMESPACE gotm::)
