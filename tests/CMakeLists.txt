find_package(GTest REQUIRED)

add_executable(allTests
        all_tests.cpp
)

target_include_directories(allTests PRIVATE ${AST_INCLUDE_DIR})

target_link_libraries(allTests PRIVATE
        GTest::GTest
)