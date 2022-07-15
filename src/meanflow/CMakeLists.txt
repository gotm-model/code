add_library(meanflow
   prognostic_buoyancy.F90
   convectiveadjustment.F90
   coriolis.F90
   external_pressure.F90
   friction.F90
   internal_pressure.F90
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
target_link_libraries(meanflow PRIVATE gsw_static stokes_drift observations airsea_driver config)
#KBtarget_link_libraries(meanflow PRIVATE stokes_drift observations airsea_driver config)
set_property(TARGET meanflow PROPERTY FOLDER gotm)
