add_library(gotm_fabm
   gotm_fabm.F90
)
target_link_libraries(gotm_fabm PUBLIC fabm PRIVATE util field_manager)
set_property(TARGET gotm_fabm PROPERTY FOLDER gotm)

if(NOT GOTM_BUILD_LIBRARIES_ONLY)
   add_library(gotm_fabm_io
               gotm_fabm_input.F90
   )
   target_link_libraries(gotm_fabm_io PRIVATE input gotm_fabm fabm)
   set_property(TARGET gotm_fabm_io PROPERTY FOLDER gotm)
endif()

install(TARGETS gotm_fabm 
   EXPORT gotmConfig
   LIBRARY DESTINATION lib 
   ARCHIVE DESTINATION lib
)
install(FILES 
   $<TARGET_PROPERTY:gotm_fabm,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/gotm_fabm.mod
   DESTINATION include
)
