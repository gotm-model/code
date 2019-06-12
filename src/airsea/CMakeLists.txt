add_library(airsea
   airsea.F90
   airsea_fluxes.F90
   airsea_variables.F90
   back_radiation.F90
   fairall.F90
   humidity.F90
   kondo.F90
   solar_zenith_angle.F90
   short_wave_radiation.F90
   albedo_water.F90
)
target_compile_definitions(airsea PRIVATE GUSTINESS)
target_link_libraries(airsea PRIVATE util input)
