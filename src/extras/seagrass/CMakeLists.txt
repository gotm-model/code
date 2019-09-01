add_library(seagrass
   seagrass.F90
)
target_link_libraries(seagrass PUBLIC field_manager PRIVATE meanflow)
set_property(TARGET seagrass PROPERTY FOLDER gotm)
