add_library(meanflow
   buoyancy.F90
   convectiveadjustment.F90
   coriolis.F90
   extpressure.F90
   friction.F90
   intpressure.F90
   meanflow.F90
   salinity.F90
   shear.F90
   stratification.F90
   temperature.F90
   uequation.F90
   updategrid.F90
   vequation.F90
   wequation.F90
)
target_link_libraries(meanflow PRIVATE observations airsea_driver config)
set_property(TARGET meanflow PROPERTY FOLDER gotm)
