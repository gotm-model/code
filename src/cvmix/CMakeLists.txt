add_library(gotm_cvmix
   gotm_cvmix.F90
)
target_link_libraries(gotm_cvmix PRIVATE cvmix_objects turbulence yaml)
set_property(TARGET gotm_cvmix PROPERTY FOLDER gotm)
