#include <iostream>
#include "tokenExpr.h"
#include "expression.h"



int main() {

    TokenExpression t{Tokenizer("sin(3+cos(2))").tokenize()};
    auto result = t.addImplMultiplication().setUnaryMinFlags().getPostfixExpression();
    for (const auto& r : result) {
        std::cout << r.getStr() << "\n";
    }

#ifdef DEBUG
    testing::InitGoogleTest();
    return RUN_ALL_TESTS();
#endif
}
