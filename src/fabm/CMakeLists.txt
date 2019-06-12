add_library(gotm_fabm
   gotm_fabm.F90
)
target_link_libraries(gotm_fabm PUBLIC fabm PRIVATE util field_manager)
set_property(TARGET gotm_fabm PROPERTY FOLDER gotm)

add_library(gotm_fabm_io
            gotm_fabm_input.F90
)
target_link_libraries(gotm_fabm_io PRIVATE input gotm_fabm fabm)
set_property(TARGET gotm_fabm_io PROPERTY FOLDER gotm)
