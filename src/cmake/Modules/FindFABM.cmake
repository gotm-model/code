# Try to locate FABM's installation prefix.
find_path(FABM_PREFIX
  NAMES include/fabm_driver.h
  HINTS "$ENV{FABM_PREFIX}"
  PATHS "$ENV{LOCALAPPDATA}/fabm/gotm" "$ENV{APPDATA}/fabm/gotm" "$ENV{HOME}/local/fabm/gotm"
  DOC "Installation prefix for Framework for Aquatic Biogeochemical Models - fabm.net"
)

# Find FABM library
find_library(FABM_LIBRARIES NAMES fabm
             HINTS ${FABM_PREFIX}/lib
             DOC "FABM libraries")

# Store configurable path of FABM include directory
find_path(FABM_INCLUDE_DIRS
          NAMES fabm_driver.h
          HINTS ${FABM_PREFIX}/include
          DOC "FABM include directory"
)

mark_as_advanced(FABM_LIBRARIES FABM_INCLUDE_DIRS)

# Process default arguments (QUIET, REQUIRED)
include(FindPackageHandleStandardArgs) 
find_package_handle_standard_args (FABM DEFAULT_MSG FABM_LIBRARIES FABM_INCLUDE_DIRS) 

# For backward compatibility:
set(FABM_LIBRARY FABM_LIBRARIES)
set(FABM_INCLUDE_DIR FABM_INCLUDE_DIRS)
