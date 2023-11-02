#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "pybind11/functional.h"

#include "expression.h"

namespace py = pybind11;

template class Expression<double>;

PYBIND11_MODULE(mathfuncs_parse, m) {
    py::class_<Expression<double>>(m, "func")
            .def(py::init<const std::string&>())
            .def(py::init<>())
            .def("init", &Expression<double>::checkInitWithExcept)
            .def("valid", &Expression<double>::isValidExpr)
            .def("eval", py::overload_cast<const std::unordered_map<std::string, double>&>(&Expression<double>::evaluate))
            .def("eval", py::overload_cast<>(&Expression<double>::evaluate))
            .def("vars", &Expression<double>::getVariables)
            .def("add_func", py::overload_cast<const std::string&, std::function<double(double)>>(&Expression<double>::addFunction));
}
