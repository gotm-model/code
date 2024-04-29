add_library(airsea_driver
   airsea.F90
)
target_compile_definitions(airsea_driver PRIVATE GUSTINESS)
target_link_libraries(airsea_driver PRIVATE util input_manager config airsea)
set_property(TARGET airsea_driver PROPERTY FOLDER gotm)

add_library(airsea
   airsea_fluxes.F90
   airsea_variables.F90
   humidity.F90
   longwave_radiation.F90
   fairall.F90
   kondo.F90
   solar_zenith_angle.F90
   albedo_water.F90
   shortwave_radiation.F90
)
target_compile_definitions(airsea PRIVATE GUSTINESS)
target_include_directories(airsea PUBLIC $<BUILD_INTERFACE:$<TARGET_PROPERTY:airsea,Fortran_MODULE_DIRECTORY>> $<INSTALL_INTERFACE:include>)
#target_link_libraries(airsea PUBLIC util input_manager config)
add_library(gotm::airsea ALIAS airsea)
set_property(TARGET airsea PROPERTY FOLDER gotm)

install(TARGETS airsea
   EXPORT gotmConfig
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
)
#install(FILES
#   $<TARGET_PROPERTY:airsea,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/airsea.mod
#   DESTINATION include
#)

