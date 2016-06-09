cmake_minimum_required(VERSION 2.8.8)

# Use solution folders in IDEs
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Create main project.
project(python_gotm Fortran)

# Specify position-independent code since we will build a shared library.
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

if(WIN32)
  add_compile_options("/libs:static")
endif()

# Include GOTM's CMakeLists.txt.
set(GOTM_BUILD_LIBRARY ON)
add_subdirectory(${PROJECT_SOURCE_DIR}/../src gotm)

# Add Python-FABM library
add_library(python_gotm SHARED
            python_gotm.F90
            ${GOTM_SOURCES}
           )

if(WIN32)
  set_property(TARGET python_gotm PROPERTY LINK_FLAGS_DEBUG "/NODEFAULTLIB:\"libcmt\"")
endif()

# Link in FABM itself
target_link_libraries(python_gotm ${GOTM_LIBS})

# Set include directories with modules (*.mod) - our own and FABM's.
set_property(TARGET python_gotm APPEND PROPERTY INCLUDE_DIRECTORIES "${CMAKE_CURRENT_BINARY_DIR}/gotm/modules")

# Set include directories with header files (*.h) - our own and FABM's.
set_property(TARGET python_gotm APPEND PROPERTY INCLUDE_DIRECTORIES "${PROJECT_SOURCE_DIR}/../include")

# Query Python itself for default installation prefix.
find_package(PythonInterp)
execute_process(COMMAND ${PYTHON_EXECUTABLE} -m site --user-site
                OUTPUT_VARIABLE SITE_PACKAGES_DIR
                OUTPUT_STRIP_TRAILING_WHITESPACE)
set(DEFAULT_PREFIX "${SITE_PACKAGES_DIR}")

# Set cache variable with installation prefix.
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  # None set - set our default prefix.
  set(CMAKE_INSTALL_PREFIX "${DEFAULT_PREFIX}" CACHE PATH "Directory to install python_fabm in" FORCE)
else()
  # User specified a prefix - just update the variable description.
  set(CMAKE_INSTALL_PREFIX "${DEFAULT_PREFIX}" CACHE PATH "Directory to install python_fabm in")
endif()
message(STATUS "Installation prefix: ${CMAKE_INSTALL_PREFIX}")

install(TARGETS python_gotm
  DESTINATION pygotm)
install(DIRECTORY pygotm.in/
  DESTINATION pygotm)

