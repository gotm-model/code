cmake_minimum_required(VERSION 3.0)

if(NOT PROJECT_NAME STREQUAL gotm)
   message(WARNING "You are using ${CMAKE_CURRENT_LIST_DIR} as source directory. This is deprecated. Please use its parent directory instead.")
   project(gotm VERSION 5.3.0 LANGUAGES Fortran)
   include("${CMAKE_CURRENT_LIST_DIR}/../CMakeLists.txt")
   return()
endif()

# ----------------------------
# Directory wide settings
# ----------------------------

# Common directory for Fortran modules
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

# Compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  add_definitions(-DGFORTRAN)
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
include_directories("${CMAKE_CURRENT_BINARY_DIR}/modules")

# Preprocessor definitions
add_definitions(-DFORTRAN95)
if(GOTM_USE_NetCDF)
   add_definitions(-DNETCDF_FMT)
endif()
if(GOTM_USE_FABM)
  add_definitions(-D_FABM_)
endif()
if(GOTM_USE_STIM)
  add_definitions(-D_ICE_)
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

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/airsea airsea)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/util util)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/turbulence turbulence)

if(GOTM_USE_FABM)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/fabm fabm)
endif()

if(GOTM_USE_CVMIX)
    add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/cvmix gotm_cvmix)
endif()

if(GOTM_BUILD_LIBRARIES_ONLY)
   return()
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/input input)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/config config)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/observations observations)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/meanflow meanflow)
add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/stokes_drift stokes_drift)

if(GOTM_USE_SEAGRASS)
   add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/extras/seagrass seagrass)
endif()

add_subdirectory(${CMAKE_CURRENT_LIST_DIR}/gotm gotmlib)

# Build GOTM executable.
add_executable(gotm_exe
   ${CMAKE_CURRENT_LIST_DIR}/gotm/main.F90
)
set_property(TARGET gotm_exe PROPERTY RUNTIME_OUTPUT_NAME "gotm")
target_link_libraries(gotm_exe PRIVATE gotm)

# Install GOTM executable
install(TARGETS gotm_exe RUNTIME DESTINATION bin)

install(EXPORT gotmConfig DESTINATION cmake NAMESPACE gotm::)
