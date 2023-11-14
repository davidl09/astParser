#include <iostream>
#include "expression.h"
#include <complex>

int main() {
    Expression<std::complex<double>> expr;

    //expr.addFunction("f", x_2_1.asUnaryFunc());
/*

    while (true) {
        std::cout << "Enter an expression \n";
        try {
            std::cin >> expr;
            expr.optimize();

            std::cout << expr.string() << " = " << expr.evaluate({{"e", std::numbers::e}, {"pi", std::numbers::pi}, {"i", {0, 1}}}) << std::endl;
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
