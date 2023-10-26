#include <iostream>
#include "tokenExpr.h"
#include "expression.h"



int main() {

    Tokenizer t("3+-3x");
    TokenExpression e{t.tokenize()};
    auto result = e.insertImplicitMult().getExpression();
    for (const auto& r : result) {
        std::cout << r.getStr() << "\n";
    }

#ifdef DEBUG
    testing::InitGoogleTest();
    return RUN_ALL_TESTS();
#endif
}
