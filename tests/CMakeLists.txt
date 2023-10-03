add_executable(testsuite
        tokenizer_test.cpp
        ast_test.cpp
)

find_package(GTest REQUIRED)
target_link_libraries(testsuite PUBLIC GTest::gtest_main)

target_include_directories(testsuite PUBLIC ${CMAKE_SOURCE_DIR})
