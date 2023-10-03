cmake_minimum_required(VERSION 3.25)
project(ast)

set(CMAKE_CXX_STANDARD 20)

add_executable(ast main.cpp)

if(${CMAKE_BUILD_TYPE} STREQUAL "")
    set(CMKAKE_BUILD_TYPE "Debug")
    message("Defaulting to Debug Mode")
endif ()

if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    add_subdirectory(tests)
    target_compile_definitions(ast PUBLIC "-DDEBUG")
    message("Building Tests")
endif ()