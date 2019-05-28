# Create main project.
#KBproject(gotm_tests VERSION 5.3.0 LANGUAGES Fortran)

#if(GOTM_TESTS)
include_directories(BEFORE ${PROJECT_SOURCE_DIR}/include)
# Can likely be done more clever using target_include_directories
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/../src/modules)

add_executable(test_airsea
               test_airsea.F90
              )
target_link_libraries(test_airsea PRIVATE util airsea input)

add_executable(test_albedo
               test_albedo.F90
              )
target_link_libraries(test_albedo PRIVATE util airsea input)

add_executable(test_eqstate
               test_eqstate.F90
              )
target_link_libraries(test_eqstate PRIVATE util)

add_executable(test_time
               test_time.F90
              )
target_link_libraries(test_time PRIVATE util)

add_custom_target(gotm_tests WORKING_DIRECTORY tests COMMENT "building tests")
add_dependencies(gotm_tests test_airsea test_albedo test_eqstate test_time)
#KBset_property(TARGET test_all test_airsea test_albedo test_eqstate test_time PROPERTY FOLDER tests)

# Dont know if we have to install these
install(TARGETS 
        test_airsea test_albedo test_eqstate test_time 
        RUNTIME DESTINATION bin
)

#endif(GOTM_TESTS)
