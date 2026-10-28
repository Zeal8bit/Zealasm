execute_process(
    COMMAND "${GIT_EXECUTABLE}" describe --always
    WORKING_DIRECTORY "${SOURCE_DIR}"
    RESULT_VARIABLE result
    OUTPUT_VARIABLE version
    OUTPUT_STRIP_TRAILING_WHITESPACE
)
if(NOT result EQUAL 0)
    message(FATAL_ERROR "Could not determine the Git version.")
endif()
file(WRITE "${OUTPUT_FILE}.tmp" "${version} ")
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${OUTPUT_FILE}.tmp" "${OUTPUT_FILE}"
    RESULT_VARIABLE result)
file(REMOVE "${OUTPUT_FILE}.tmp")
if(NOT result EQUAL 0)
    message(FATAL_ERROR "Could not write ${OUTPUT_FILE}.")
endif()
