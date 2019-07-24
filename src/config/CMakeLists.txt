add_library(config
   settings.F90
)
target_link_libraries(config PUBLIC yaml PRIVATE input_manager)
set_property(TARGET config PROPERTY FOLDER gotm)

install(TARGETS config
   EXPORT gotmConfig
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
)

