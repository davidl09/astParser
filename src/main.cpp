#include <iostream>
#include "expression.h"
#include <complex>

int main() {
    Expression<double> expr;

    //expr.addFunction("f", x_2_1.asUnaryFunc());
/*
    while (true) {
        std::cout << "Enter an expression \n";
        try {
            std::cin >> expr;
            expr.optimize();

            std::cout << expr.string() << " = " << expr.evaluate({{"e", std::numbers::e}, {"pi", std::numbers::pi}}) << std::endl;
        }
        catch(std::exception& e) {
            std::cout << "Error: " << e.what() << "\n";
        }
    }
    */
    while (true) {
        std::cout << "Enter an expression \n";
        std::cin >> expr;
        auto res = expr.derivative("x");

        std::cout << res.string() << "\n";
    }
}
