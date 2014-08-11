# Try to locate FABM's installation prefix.
find_path(FABM_INSTALL_PREFIX
  NAMES include/fabm_driver.h
  PATHS "$ENV{LOCALAPPDATA}/fabm/gotm" "$ENV{APPDATA}/fabm/gotm" "$ENV{HOME}/local/fabm/gotm"
)

# Find FABM library
find_library(FABM_LIBRARIES NAMES fabm
             HINTS ${FABM_INSTALL_PREFIX}/lib
             DOC "FABM libraries")

# Store configurable path of FABM include directory
set(FABM_INCLUDE_DIRS "${FABM_INSTALL_PREFIX}/include"
    CACHE PATH
    "FABM include directories")

mark_as_advanced(FABM_LIBRARIES FABM_INCLUDE_DIRS)

# Process default arguments (QUIET, REQUIRED)
include(FindPackageHandleStandardArgs) 
find_package_handle_standard_args (NetCDF DEFAULT_MSG FABM_LIBRARIES FABM_INCLUDE_DIRS) 

# For backward compatibility:
set(FABM_LIBRARY FABM_LIBRARIES)
set(FABM_INCLUDE_DIR FABM_INCLUDE_DIRS)
