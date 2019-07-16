add_library(config
#   settings.F90
   yaml_settings.F90
)
target_link_libraries(config PRIVATE yaml)
set_property(TARGET config PROPERTY FOLDER gotm)

install(TARGETS config
   EXPORT gotmConfig
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
)

