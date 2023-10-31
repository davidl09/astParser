#include "pybind11/pybind11.h"
#include "pybind11/stl.h"

#include "expression.h"

namespace py = pybind11;

/*
class doubleExpression {
public:
    explicit doubleExpression(const std::string &e)
            : expr(e) {}

    double evaluate() {
        return expr.evaluate();
    }

private:
    Expression<double> expr;
};
*/

template class Expression<double>;

PYBIND11_MODULE(PyParser, m) {
    py::class_<Expression<double>>(m, "Expression")
            .def(py::init<const std::string&>())
            .def(py::init<>())
            .def("init", &Expression<double>::checkInitWithExcept)
            .def("validate", &Expression<double>::isValidExpr)
            .def("eval", py::overload_cast<const std::unordered_map<std::string, double>&>(&Expression<double>::evaluate))
            .def("eval", py::overload_cast<>(&Expression<double>::evaluate));
}
