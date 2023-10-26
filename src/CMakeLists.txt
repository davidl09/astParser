add_executable(ast
        main.cpp
        ../include/tokenExpr.h
)

target_include_directories(ast PUBLIC ${AST_INCLUDE_DIR})