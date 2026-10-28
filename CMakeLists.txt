cmake_minimum_required(VERSION 3.16)

if(NOT EXISTS "$ENV{ZOS_PATH}/cmake/zos_init.cmake")
    message(FATAL_ERROR "Set ZOS_PATH to a current Zeal 8-bit OS checkout.")
endif()

set(ZOS_TOOLCHAIN z88dk)
include("$ENV{ZOS_PATH}/cmake/zos_init.cmake")
project(zealasm ASM)

find_package(Git REQUIRED)
add_custom_target(version
    COMMAND "${CMAKE_COMMAND}"
        "-DGIT_EXECUTABLE=${GIT_EXECUTABLE}"
        "-DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
        "-DOUTPUT_FILE=${CMAKE_CURRENT_BINARY_DIR}/version.txt"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/version.cmake"
    BYPRODUCTS "${CMAKE_CURRENT_BINARY_DIR}/version.txt"
    VERBATIM
)

add_executable(zealasm
    src/main.asm
    src/file.asm
    src/parser.asm
    src/strutils.asm
    src/data.asm
)
add_dependencies(zealasm version)
target_include_directories(zealasm PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/src" "${CMAKE_CURRENT_BINARY_DIR}")

# z80asm does not provide CMake dependency scanning for INCLUDE or INCBIN.
set(zos_sdk "$ENV{ZOS_PATH}/sdk/z88dk-z80asm")
set_source_files_properties(src/main.asm PROPERTIES OBJECT_DEPENDS
    "${CMAKE_CURRENT_BINARY_DIR}/version.txt;${CMAKE_CURRENT_SOURCE_DIR}/src/assembler_macros_h.asm;${zos_sdk}/zos_sys.asm;${zos_sdk}/zos_err.asm")
set_source_files_properties(src/parser.asm PROPERTIES OBJECT_DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/src/assembler_macros_h.asm;${zos_sdk}/zos_sys.asm;${zos_sdk}/zos_err.asm")
set_source_files_properties(src/file.asm PROPERTIES OBJECT_DEPENDS "${zos_sdk}/zos_sys.asm;${zos_sdk}/zos_err.asm")
set_source_files_properties(src/data.asm PROPERTIES OBJECT_DEPENDS "${zos_sdk}/zos_err.asm")

target_link_options(zealasm PRIVATE -m)
set_property(TARGET zealasm APPEND PROPERTY ADDITIONAL_CLEAN_FILES "${CMAKE_CURRENT_BINARY_DIR}/zealasm.map")
