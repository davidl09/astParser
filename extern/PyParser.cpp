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

PYBIND11_MODULE(pyparser, m) {
    py::class_<Expression<double>>(m, "Expression")
            .def(py::init<const std::string&>())
            .def("eval", py::overload_cast<>(&Expression<double>::evaluate));
}
