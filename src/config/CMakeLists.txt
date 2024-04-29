add_library(config
   settings.F90
)
target_include_directories(config PUBLIC $<BUILD_INTERFACE:$<TARGET_PROPERTY:config,Fortran_MODULE_DIRECTORY>> $<INSTALL_INTERFACE:include>)
target_link_libraries(config PUBLIC yaml PRIVATE input_manager)
add_library(gotm::config ALIAS config)
set_property(TARGET config PROPERTY FOLDER gotm)

install(TARGETS config
   EXPORT gotmConfig
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
)

