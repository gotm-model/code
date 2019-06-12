add_library(input
   input.F90
   input_netcdf.F90
)
target_link_libraries(input PRIVATE util output_manager)
target_include_directories(input PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules> $<INSTALL_INTERFACE:include>)
