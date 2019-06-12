add_library(seagrass
   seagrass.F90
)
target_link_libraries(seagrass PRIVATE meanflow field_manager)
set_property(TARGET seagrass PROPERTY FOLDER gotm)
