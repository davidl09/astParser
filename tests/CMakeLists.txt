find_package(GTest REQUIRED)

add_executable(allTests
        all_tests.cpp
)

target_include_directories(allTests PRIVATE ${AST_INCLUDE_DIR})

gtest_add_tests(
        TARGET allTests
        TEST_LIST all_gtest_tests
)


message("Found Tests: ${all_gtest_tests}")

set_tests_properties(
        ${all_gtest_tests} PROPERTIES TIMEOUT 20
)

add_test(
        NAME all_gtest
        COMMAND .${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/allTests
)

target_link_libraries(allTests PRIVATE
        GTest::GTest
)