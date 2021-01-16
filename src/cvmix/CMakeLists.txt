add_library(gotm_cvmix
   $<TARGET_OBJECTS:cvmix_objects>
   gotm_cvmix.F90
)
target_link_libraries(gotm_cvmix PRIVATE turbulence yaml)
set_property(TARGET gotm_cvmix PROPERTY FOLDER gotm)
