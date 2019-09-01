# Generate source file with compilation information
configure_file(compilation.F90.in compilation.F90)

# Create placeholder file with version information, used when GOTM_EMBED_VERSION is off.
set(GIT_COMMIT_ID ${gotm_VERSION})
set(GIT_BRANCH_NAME unknown)
configure_file(gotm_version.F90.in gotm_version.F90)

add_library(util
   adv_center.F90
   convert_fluxes.F90
   diff_center.F90
   diff_face.F90
   eqstate.F90
   gridinterpol.F90
   lagrange.F90
   ode_solvers.F90
   time.F90
   tridiagonal.F90
   util.F90
   ../../include/cppdefs.h
   ${CMAKE_CURRENT_BINARY_DIR}/gotm_version.F90
   ${CMAKE_CURRENT_BINARY_DIR}/compilation.F90
)
target_link_libraries(util PRIVATE yaml)
target_include_directories(util PUBLIC $<BUILD_INTERFACE:$<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>> $<INSTALL_INTERFACE:include>)
add_library(gotm::util ALIAS util)
set_property(TARGET util PROPERTY FOLDER gotm)

if(GOTM_EMBED_VERSION)
   # Create a custom target for generating version  information (only if GOTM_EMBED_VERSION is on)
   add_custom_target(version
      ${CMAKE_COMMAND} -DINFILE=${CMAKE_CURRENT_SOURCE_DIR}/gotm_version.F90.in -DOUTFILE=${CMAKE_CURRENT_BINARY_DIR}/gotm_version.F90 -P "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/Modules/GetGitInfo.cmake"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      COMMENT "Retrieving description of last GOTM commit..."
      SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/gotm_version.F90.in "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/Modules/GetGitInfo.cmake"
      VERBATIM
   )
   add_dependencies(util version)
   set_property(TARGET version PROPERTY FOLDER gotm)
endif()

install(TARGETS util 
   EXPORT gotmConfig
   LIBRARY DESTINATION lib 
   ARCHIVE DESTINATION lib
)
install(FILES 
   $<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/time.mod
   $<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/util.mod
   $<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/gotm_version.mod
   $<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/gotm_compilation.mod
   $<TARGET_PROPERTY:util,Fortran_MODULE_DIRECTORY>/\${BUILD_TYPE}/mtridiagonal.mod
   DESTINATION include
)
