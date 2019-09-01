add_library(observations
   analytical_profile.F90
   const_NNS.F90
   const_NNT.F90
   observations.F90
)
target_link_libraries(observations PRIVATE util input_manager config)
set_property(TARGET observations PROPERTY FOLDER gotm)
