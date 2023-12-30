#include <iostream>
#include "expression.h"
#include <complex>

int main() {
    Expression<double> expr;

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
    }q
    */

    while (true) {
        std::cout << "Enter an expression \n";
        try {
            std::cin >> expr;
            auto res = expr.derivative("x");
            std::cout << res.string() << "\n";
        } catch (std::exception& e) {
            std::cout << e.what() << '\n';
        }
    }
}
