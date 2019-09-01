add_library(gotm_fabm
   gotm_fabm.F90
)
target_link_libraries(gotm_fabm PUBLIC fabm field_manager PRIVATE util yaml)
target_include_directories(gotm_fabm PUBLIC $<BUILD_INTERFACE:$<TARGET_PROPERTY:gotm_fabm,Fortran_MODULE_DIRECTORY>> $<INSTALL_INTERFACE:include>)
add_library(gotm::gotm_fabm ALIAS gotm_fabm)
set_property(TARGET gotm_fabm PROPERTY FOLDER gotm)

add_library(gotm_fabm_io
            gotm_fabm_input.F90
)
target_link_libraries(gotm_fabm_io PRIVATE input_manager gotm_fabm fabm config)
set_property(TARGET gotm_fabm_io PROPERTY FOLDER gotm)

install(TARGETS gotm_fabm 
   EXPORT gotmConfig
   LIBRARY DESTINATION lib 
   ARCHIVE DESTINATION lib
)
install(FILES 
   $<TARGET_PROPERTY:gotm_fabm,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/gotm_fabm.mod
   DESTINATION include
)
