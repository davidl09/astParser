cmake_minimum_required(VERSION 3.25)
project(ast)

set(CMAKE_CXX_STANDARD 20)

set(AST_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/include")


if(${CMAKE_BUILD_TYPE} STREQUAL "")
    set(CMAKE_BUILD_TYPE "Debug")
    message("Defaulting to Debug Mode")
endif ()

if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    message("Debug Build")
    add_compile_definitions("DEBUG")
    add_compile_options("-Og")
    message("Building Tests")
    find_package(GTest REQUIRED)

elseif(${CMAKE_BUILD_TYPE} STREQUAL "Release")
    message("Release Build")
    add_compile_options("-O3")
    add_compile_definitions("RELEASE")
endif ()

add_subdirectory(src)

target_link_libraries(ast PUBLIC tbb)

if(${CMAKE_BUILD_TYPE} STREQUAL Debug)
    target_link_libraries(ast PRIVATE
            GTest::GTest
    )
endif ()