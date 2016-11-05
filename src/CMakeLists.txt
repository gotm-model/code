cmake_minimum_required(VERSION 2.8)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules/")

# Create main project.
project(gotm Fortran)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Set version information.
set(gotm_VERSION_MAJOR 4)
set(gotm_VERSION_MINOR 1)
set(gotm_VERSION_PATCH 0)
set(gotm_VERSION "${gotm_VERSION_MAJOR}.${gotm_VERSION_MINOR}.${gotm_VERSION_PATCH}")

if(GOTM_BUILD_LIBRARY)
  set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
endif()

set(GIT_COMMIT_ID ${gotm_VERSION})
set(GIT_BRANCH_NAME unknown)
configure_file("${PROJECT_SOURCE_DIR}/util/compilation.F90.in" compilation.F90)
configure_file("${PROJECT_SOURCE_DIR}/util/gotm_version.F90.in" gotm_version.F90)
option(GOTM_EMBED_VERSION "Embed GOTM version information" OFF)
if(GOTM_EMBED_VERSION)
  add_custom_target(version
    ${CMAKE_COMMAND} -DINFILE=${PROJECT_SOURCE_DIR}/util/gotm_version.F90.in -DOUTFILE=${CMAKE_CURRENT_BINARY_DIR}/gotm_version.F90 -P "${PROJECT_SOURCE_DIR}/cmake/Modules/GetGitInfo.cmake"
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMENT "Retrieving description of last GOTM commit..."
    SOURCES ${PROJECT_SOURCE_DIR}/util/gotm_version.F90.in "${PROJECT_SOURCE_DIR}/cmake/Modules/GetGitInfo.cmake"
    VERBATIM
  )
  set_property(TARGET version PROPERTY FOLDER gotm)
endif()

# Specify default build type for single-type systems (not VS)
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set (CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Customize compiler flags
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Cray")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -N 1023")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
  if(WIN32)
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} /Od")
  endif()
endif()

# Use use position-independent code (-fPIC) everywhere if building shared libraries
if(BUILD_SHARED_LIBS)
  set(CMAKE_POSITION_INDEPENDENT_CODE ON)
endif()

# Global preprocessor definitions
add_definitions(-DFORTRAN95)

option(GOTM_EXTRA_OUTPUT "Include additional turbulence diagnostics in output" OFF)
mark_as_advanced(GOTM_EXTRA_OUTPUT)
if(GOTM_EXTRA_OUTPUT)
  add_definitions(-DEXTRA_OUTPUT)
endif()

option(GOTM_USE_FABM "Include support for Framework for Aquatic Biogeochemical Models (fabm.net)" ON)
if(GOTM_USE_FABM)
  add_definitions(-D_FABM_)
endif()

option(GOTM_USE_FLEXIBLE_OUTPUT "Use new output manager" ON)
if(GOTM_USE_FLEXIBLE_OUTPUT)
  add_definitions(-D_FLEXIBLE_OUTPUT_)
endif()

if(NOT GOTM_BUILD_LIBRARIES_ONLY OR GOTM_USE_FLEXIBLE_OUTPUT)
  option(GOTM_USE_NetCDF "Enable output in NetCDF format" ON)
  if(GOTM_USE_NetCDF)
    find_package(NetCDF REQUIRED)
    add_definitions(-DNETCDF_FMT -DREAL_4B=real\(4\))
    include_directories("${NetCDF_INCLUDE_DIRS}")
    if (NetCDF_STATIC_MSVC_BUILD)
      # On Windows with a statically-compiled NetCDF library - compile all code against static runtime.
      # This MUST be done before any targets are added.
      add_compile_options("/libs:static")
    endif()
  endif(GOTM_USE_NetCDF)
endif()

# Global include directories
include_directories(BEFORE "${CMAKE_BINARY_DIR}"
                    "${PROJECT_SOURCE_DIR}/../include"
                    "${PROJECT_SOURCE_DIR}/util"
                    "${CMAKE_CURRENT_BINARY_DIR}/modules"
                   )

add_library(util
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
            util/field_manager.F90
            ../include/cppdefs.h
            ${CMAKE_CURRENT_BINARY_DIR}/gotm_version.F90
            ${CMAKE_CURRENT_BINARY_DIR}/compilation.F90
           )
set_property(TARGET util PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
if(GOTM_EMBED_VERSION)
  add_dependencies(util version)
endif()

add_library(turbulence
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
target_link_libraries(turbulence util)
set_property(TARGET turbulence PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

set_property(TARGET util turbulence PROPERTY FOLDER gotm)

if(GOTM_USE_FABM)
  find_path(FABM_BASE src/fabm.F90 DOC "Path to FABM source directory.")
  mark_as_advanced(CLEAR FABM_BASE)
  if(FABM_BASE)
    # Build FABM from source by including its CMakeLists.txt directory.
    set(FABM_EMBED_VERSION ${GOTM_EMBED_VERSION} CACHE BOOL "Embed FABM version information" FORCE)
    set(FABM_FORCED_HOST gotm)
    add_subdirectory(${FABM_BASE}/src ${CMAKE_CURRENT_BINARY_DIR}/fabm_src)
    set(FABM_LIBRARIES fabm)
    set(FABM_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/fabm_src/modules)
    mark_as_advanced(FORCE FABM_PREFIX FABM_EMBED_VERSION)
  else()
    # FABM must be pre-built: use FABM_PREFIX to locate existing include and lib directories.
    find_package(FABM REQUIRED)
    mark_as_advanced(CLEAR FABM_PREFIX)
  endif()
  add_library(gotm_fabm
              fabm/gotm_fabm.F90
             )
  target_link_libraries(gotm_fabm util)
  set_property(TARGET gotm_fabm APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  set_property(TARGET gotm_fabm APPEND PROPERTY INTERFACE_INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  set_property(TARGET gotm_fabm PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
  set_property(TARGET gotm_fabm PROPERTY FOLDER gotm)
  target_link_libraries(gotm_fabm ${FABM_LIBRARIES})
else()
  mark_as_advanced(FORCE FABM_PREFIX FABM_BASE)
endif(GOTM_USE_FABM)

if(GOTM_USE_FLEXIBLE_OUTPUT)
  if(NOT GOTM_USE_FABM)
    add_subdirectory(yaml)
    set_property(TARGET yaml test_yaml PROPERTY FOLDER yaml)
    set_property(TARGET yaml PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
    set(YAML_SOURCES $<TARGET_OBJECTS:yaml>)
  endif()
  add_library(output_manager
              ${YAML_SOURCES}
              output/output_manager.F90
              output/output_manager_core.F90
              output/netcdf_output.F90
              output/text_output.F90
             )
  set_property(TARGET output_manager PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
  set_property(TARGET output_manager PROPERTY FOLDER gotm)
  target_link_libraries(output_manager util)
  if(GOTM_USE_FABM)
    target_link_libraries(output_manager ${FABM_LIBRARIES})
    set_property(TARGET output_manager APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  endif()
endif(GOTM_USE_FLEXIBLE_OUTPUT)

if(NOT GOTM_BUILD_LIBRARIES_ONLY)

add_library(input OBJECT
            input/input.F90
           )
add_dependencies(input util)

add_library(airsea OBJECT
            airsea/airsea.F90
            airsea/airsea_fluxes.F90
            airsea/airsea_variables.F90
            airsea/back_radiation.F90
            airsea/fairall.F90
            airsea/humidity.F90
            airsea/kondo.F90
            airsea/solar_zenith_angle.F90
            airsea/short_wave_radiation.F90
            airsea/albedo_water.F90
           )
set_property(TARGET airsea APPEND PROPERTY COMPILE_DEFINITIONS GUSTINESS)
add_dependencies(airsea util input)

add_library(observations OBJECT
            observations/analytical_profile.F90
            observations/const_NNS.F90
            observations/const_NNT.F90
            observations/observations.F90
           )
add_dependencies(observations util input)

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
add_dependencies(meanflow util observations airsea)

if(NOT GOTM_USE_FLEXIBLE_OUTPUT)
  add_library(output OBJECT
              output/asciiout.F90
              output/ncdfout.F90
              output/output.F90
             )
  add_dependencies(output util meanflow turbulence observations airsea)
  set_property(TARGET output PROPERTY FOLDER gotm)
endif(NOT GOTM_USE_FLEXIBLE_OUTPUT)

add_library(gotm OBJECT
            gotm/gotm.F90
            gotm/diagnostics.F90
            gotm/register_all_variables.F90
            gotm/print_version.F90
           )
add_dependencies(gotm input util meanflow turbulence observations airsea)
if(GOTM_USE_FLEXIBLE_OUTPUT)
  add_dependencies(gotm output_manager)
else()
  add_dependencies(gotm output)
  set(EXTRA_SRCS ${EXTRA_SRCS} $<TARGET_OBJECTS:output>)
endif()

set_property(TARGET input airsea observations meanflow gotm PROPERTY FOLDER gotm)

#option(GOTM_USE_ICE "Include ICE support" OFF)
#if(GOTM_USE_ICE)
#  add_library(ice OBJECT
#             )
#endif(GOTM_USE_ICE)

option(GOTM_USE_SEAGRASS "Enable seagrass module" OFF)
if(GOTM_USE_SEAGRASS)
  add_definitions(-DSEAGRASS)
  add_library(seagrass OBJECT
              extras/seagrass/seagrass.F90
             )
  set(EXTRA_SRCS ${EXTRA_SRCS} $<TARGET_OBJECTS:seagrass>)
  add_dependencies(seagrass output meanflow)
  add_dependencies(gotm seagrass)
  set_property(TARGET seagrass PROPERTY FOLDER gotm)
endif()

if(GOTM_USE_FABM)
  if(GOTM_USE_FLEXIBLE_OUTPUT)
    add_library(gotm_fabm_io
                fabm/gotm_fabm_input.F90
               )
  else()
    add_library(gotm_fabm_io
                fabm/gotm_fabm_input.F90
                fabm/gotm_fabm_output.F90
               )
    add_dependencies(gotm_fabm_io output)
  endif()
  set_property(TARGET gotm_fabm_io APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  add_dependencies(gotm_fabm_io input gotm_fabm)
  add_dependencies(gotm gotm_fabm gotm_fabm_io)
  set_property(TARGET gotm APPEND PROPERTY INCLUDE_DIRECTORIES "${FABM_INCLUDE_DIRS}")
  set_property(TARGET gotm_fabm_io PROPERTY FOLDER gotm)
endif(GOTM_USE_FABM)

set(EXTRA_SRCS
  $<TARGET_OBJECTS:gotm>
  $<TARGET_OBJECTS:airsea>
  $<TARGET_OBJECTS:meanflow>
  $<TARGET_OBJECTS:observations>
  $<TARGET_OBJECTS:input>
  ${EXTRA_SRCS}
)

if(NOT GOTM_BUILD_LIBRARY)
# Build GOTM executable.
add_executable(gotm_exe
               gotm/main.F90
               ${EXTRA_SRCS}
              )
set_property(TARGET gotm_exe PROPERTY OUTPUT_NAME "gotm")
endif()

set(EXTRA_LIBS turbulence util)
if(GOTM_USE_FLEXIBLE_OUTPUT)
  set(EXTRA_LIBS ${EXTRA_LIBS} output_manager)
endif()

if(GOTM_USE_NetCDF)
  set(EXTRA_LIBS ${EXTRA_LIBS} "${NetCDF_LIBRARIES}")
  if (NetCDF_STATIC_MSVC_BUILD AND NOT GOTM_BUILD_LIBRARY)
    set_property(TARGET gotm_exe PROPERTY LINK_FLAGS_DEBUG "/NODEFAULTLIB:\"libcmt\"")
  endif()
endif(GOTM_USE_NetCDF)

if(GOTM_USE_FABM)
  set(EXTRA_LIBS ${EXTRA_LIBS} gotm_fabm gotm_fabm_io ${FABM_LIBRARIES})
endif(GOTM_USE_FABM)

if(GOTM_BUILD_LIBRARY)
  set(GOTM_SOURCES ${EXTRA_SRCS} PARENT_SCOPE)
  set(GOTM_LIBS ${EXTRA_LIBS} PARENT_SCOPE)
else(GOTM_BUILD_LIBRARY)
target_link_libraries(gotm_exe ${EXTRA_LIBS})

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
install(TARGETS gotm_exe DESTINATION bin)
install(TARGETS turbulence util DESTINATION lib)
if(GOTM_USE_FABM)
  install(TARGETS gotm_fabm DESTINATION lib)
endif()
if(GOTM_USE_FLEXIBLE_OUTPUT)
  install(TARGETS output_manager DESTINATION lib)
endif()
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/\${BUILD_TYPE}/ DESTINATION include)

# Begin testing section

add_executable(test_airsea EXCLUDE_FROM_ALL
               airsea/test_airsea.F90
               $<TARGET_OBJECTS:airsea>
               $<TARGET_OBJECTS:input>
              )
target_link_libraries(test_airsea util)

add_executable(test_albedo EXCLUDE_FROM_ALL
               airsea/test_albedo.F90
               $<TARGET_OBJECTS:airsea>
               $<TARGET_OBJECTS:input>
              )
target_link_libraries(test_albedo util)

add_executable(test_eqstate EXCLUDE_FROM_ALL
               util/test_eqstate.F90
              )
target_link_libraries(test_eqstate util)

add_executable(test_time EXCLUDE_FROM_ALL
               util/test_time.F90
              )
target_link_libraries(test_time util)

add_custom_target(test_all WORKING_DIRECTORY tests COMMENT "building tests")
add_dependencies( test_all test_airsea test_eqstate test_time)

# End of testing section

endif(GOTM_BUILD_LIBRARY)
endif(NOT GOTM_BUILD_LIBRARIES_ONLY)
