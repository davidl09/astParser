#include <iostream>
#include "tokenExpr.h"
#include "expression.h"
#include <complex>

int main() {
    Expression<std::complex<double>> expr;
    while (true) {
        std::cout << "Enter an expression \n";
        std::cin >> expr;
        std::cout << expr.evaluate({{"pi", M_PI}, {"i", {0, 1}}}) << "\n";
    }
}
