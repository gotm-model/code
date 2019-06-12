add_library(seagrass
   seagrass.F90
)
target_link_libraries(seagrass PRIVATE meanflow field_manager)
target_include_directories(seagrass PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules> $<INSTALL_INTERFACE:include>)
set_property(TARGET seagrass PROPERTY FOLDER gotm)
