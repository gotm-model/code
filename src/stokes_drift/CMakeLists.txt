add_library(stokes_drift
   stokes_drift_theory.F90
   stokes_drift_exp.F90
   stokes_drift.F90
)
target_link_libraries(stokes_drift PRIVATE input_manager config)
set_property(TARGET stokes_drift PROPERTY FOLDER gotm)
