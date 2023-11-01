cmake_minimum_required(VERSION 3.25)
project(ast)

set(CMAKE_CXX_STANDARD 20)

set(AST_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/include")


if("${CMAKE_BUILD_TYPE}" MATCHES "Debug")

    message("Debug Build")
    add_compile_definitions("DEBUG")
    add_compile_options("-g")
    add_compile_options("-O0")
    add_compile_options("-Wall" "-Werror")
    add_subdirectory(tests)

elseif(${CMAKE_BUILD_TYPE} MATCHES "Release")

    message("Release Build")
    add_compile_options("-O3")
    add_compile_definitions("RELEASE")
    add_subdirectory(src)

endif ()

if(${CMAKE_BUILD_PYMODULE})
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/extern/python")
    message("Building Python Module PyParser")
    add_compile_definitions("BUILD_PYMODULE")
    add_subdirectory(extern)
endif()
