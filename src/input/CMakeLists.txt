add_library(input_manager
   input.F90
   input_netcdf.F90
)
target_link_libraries(input_manager PRIVATE util output_manager yaml)
set_property(TARGET input_manager PROPERTY FOLDER gotm)

install(TARGETS input_manager
   EXPORT gotmConfig
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
)

