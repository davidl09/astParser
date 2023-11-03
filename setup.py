from setuptools import setup, Extension

# Define the extension module
extension_module = Extension(
    name='mathfuncs_parse',
    sources=['extern/PyParser.cpp'],
    include_dirs=['extern/pybind11/include', 'include'],
    extra_compile_args=['-std=c++20'],
)

setup(
    name='mathfuncs-parse',
    version='1.1.2',
    description='A small module for parsing and evaluating expressions of any number of variables',
    author='David Laeer',
    author_email='davidlaeer@gmail.com',
    ext_modules=[extension_module],
    long_description="""
Mathfuncs Parse
===============

How to Use:
------------

After importing 'mathfuncs_parse', create a 'mathfuncs_parse.func(expr: str)' object.

Available Member Functions:
---------------------------

- ``valid() -> bool``
  Checks whether the expression is syntactically correct.

- ``eval() -> float``
  Evaluates the expression. Note: only works if no variables are present.

- ``eval(vars: dict{str: float|int}) -> float``
  Evaluates the expression, substituting the variables present with the values passed in the dict.

- ``vars() -> dict{str: float}``
  Returns a dict containing all variables that must be passed to eval().

- ``add_func(name: str, func: <functionObj(float) -> float>) -> Null``
  Adds a user-defined function to the object's internal state. Note: calling 'init(str)' does not reset this.

- ``avail_funcs() -> dict{str: <functionObj>}``
  Returns a dict of available functions which can be used in the current expression.
""",
)
