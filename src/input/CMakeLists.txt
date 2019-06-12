add_library(input
   input.F90
   input_netcdf.F90
)
target_link_libraries(input PRIVATE util output_manager)
set_property(TARGET input PROPERTY FOLDER gotm)
