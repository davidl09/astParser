add_subdirectory(pybind11)

pybind11_add_module(PyParser PyParser.cpp)

target_include_directories(PyParser PUBLIC ${AST_INCLUDE_DIR})

target_compile_options(PyParser PUBLIC "-fPIC" "-shared")
target_compile_options(PyParser PUBLIC "-fvisibility=hidden")