#include <iostream>
#include "tokenExpr.h"
#include "expression.h"



int main() {

/*
    Expression<double> e("-5+2");

    std::cout << e.evaluate({{"x", 0}}) << "\n";
*/

#ifdef DEBUG
    testing::InitGoogleTest();
    return RUN_ALL_TESTS();
#endif
}
