#include <iostream>
#include "tokenExpr.h"
#include "expression.h"
#include <complex>

int main() {
    Expression<std::complex<double>> expr;
    Expression<std::complex<double>> x_2_1("x^2+1");

    //expr.addFunction("f", x_2_1.asUnaryFunc());

    while (true) {
        std::cout << "Enter an expression \n";
        try {
            std::cin >> expr;
            std::cout << expr.evaluate({{"e", std::numbers::e}, {"pi", std::numbers::pi}, {"i", {0, 1}}}) << std::endl;
        }
        catch(std::exception& e) {
            std::cout << "Error: " << e.what() << "\n";
        }
    }
}
