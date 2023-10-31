add_subdirectory(pybind11)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "python")

pybind11_add_module(pyparser PyParser.cpp)

target_include_directories(pyparser PUBLIC ${AST_INCLUDE_DIR})

target_compile_options(pyparser PUBLIC "-fPIC" "-shared")
target_compile_options(pyparser PUBLIC "-fvisibility=hidden")