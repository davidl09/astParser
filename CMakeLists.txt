cmake_minimum_required(VERSION 3.25)
project(ast)

set(CMAKE_CXX_STANDARD 20)

set(AST_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/include")

if("${CMAKE_BUILD_TYPE}" MATCHES "Debug")

    message("Debug Build")
    add_compile_definitions("DEBUG")
    add_subdirectory(src)

elseif(${CMAKE_BUILD_TYPE} MATCHES "Release")

    message("Release Build")
    add_compile_definitions("RELEASE")
    add_subdirectory(src)

endif ()

if(${CMAKE_BUILD_PYMODULE})
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/extern/python")
    message("Building Python Module PyParser")
    add_compile_definitions("BUILD_PYMODULE")
    add_subdirectory(extern)
endif()

if(${CMAKE_RUN_GTEST})
    add_subdirectory(tests)
endif()

if(MSVC)
    add_compile_options(/W4 /WX)
else()
    add_compile_options(-Wall -Wextra -Wpedantic -Werror)
endif()

