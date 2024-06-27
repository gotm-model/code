add_library(seagrass
   seagrass.F90
)
set_property(TARGET seagrass PROPERTY FOLDER gotm)
target_link_libraries(seagrass PUBLIC field_manager PRIVATE meanflow gotm::config)
