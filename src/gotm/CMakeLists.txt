add_library(gotm
   gotm.F90
   diagnostics.F90
   register_all_variables.F90
   deprecated_output.F90
   print_version.F90
)
target_link_libraries(gotm PUBLIC util airsea input yaml meanflow turbulence output_manager input)
if(GOTM_USE_FABM)
   target_link_libraries(gotm PUBLIC gotm_fabm gotm_fabm_io)
endif()
if(GOTM_USE_SEAGRASS)
   target_link_libraries(gotm PUBLIC seagrass)
endif()
