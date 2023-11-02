from setuptools import setup, Extension

# Define the extension module
extension_module = Extension(
    name='PyParser',
    sources=['extern/PyParser.cpp'],  # Add other source files if necessary
    include_dirs=['extern/pybind11/include', 'include'],
    extra_compile_args=['-std=c++20'],  # Adjust the C++ standard flag as needed
)

setup(
    name='PyParser',
    version='1.0',
    ext_modules=[extension_module],
)
