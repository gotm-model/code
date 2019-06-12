add_library(observations
   analytical_profile.F90
   const_NNS.F90
   const_NNT.F90
   observations.F90
)
target_link_libraries(observations PRIVATE util input)
target_include_directories(observations PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules> $<INSTALL_INTERFACE:include>)
