# can maybe be done more pretty -  the problem is that otsp does not have a CMakeLists.txt file
if(GOTM_USE_OTPS)
   set (OTPS ${OTPS_BASE}/constit.h 
             ${OTPS_BASE}/derived_types.h 
             ${OTPS_BASE}/subs.f90)
   add_definitions(-D_OTPS_)
endif()
add_library(observations
   analytical_profile.F90
   const_NNS.F90
   const_NNT.F90
   observations.F90
   ${OTPS}
)
target_link_libraries(observations PRIVATE util input_manager)
set_property(TARGET observations PROPERTY FOLDER gotm)

#if(GOTM_USE_OTPS)
#   remove_definitions(-D_OTPS_)
#endif()

#if(GOTM_USE_OTPS)
#   add_subdirectory("${OTPS_BASE}" otps)
#   target_link_libraries(observations PRIVATE otps)
#endif()

#add_subdirectory(otps)
#target_link_libraries(observations PRIVATE otps)
