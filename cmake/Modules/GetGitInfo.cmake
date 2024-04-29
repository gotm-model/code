find_package(Git QUIET)
if(GIT_FOUND)
  execute_process(COMMAND ${GIT_EXECUTABLE} describe --always --dirty
                  OUTPUT_VARIABLE GIT_COMMIT_ID
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  ERROR_QUIET
                 )
  execute_process(COMMAND ${GIT_EXECUTABLE} name-rev --name-only HEAD
                  OUTPUT_VARIABLE GIT_BRANCH_NAME
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                  ERROR_QUIET
                 )
endif()
message(STATUS "On branch ${GIT_BRANCH_NAME} with commit id ${GIT_COMMIT_ID}")
configure_file("${INFILE}" "${OUTFILE}")
