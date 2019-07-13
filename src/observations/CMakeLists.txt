if(GOTM_USE_OTPS)
   add_library(otps2
      ${OTPS_BASE}/constit.h 
      ${OTPS_BASE}/derived_types.h 
      ${OTPS_BASE}/subs.f90
   )
   add_definitions(-D_OTPS_)
   include_directories(AFTER ${OTPS_BASE})
   set (OTPS gotm_otps.F90)
endif()

add_library(observations
   analytical_profile.F90
   const_NNS.F90
   const_NNT.F90
   observations.F90
   ${OTPS}
)
target_link_libraries(observations PRIVATE util input_manager)
if(GOTM_USE_OTPS)
   target_link_libraries(observations PRIVATE otps2)
endif()
set_property(TARGET observations PROPERTY FOLDER gotm)
