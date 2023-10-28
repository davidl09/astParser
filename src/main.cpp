#include <iostream>
#include "tokenExpr.h"
#include "expression.h"



int main() {

    Tokenizer t("3sin(2-3x)");
    TokenExpression e{t.tokenize()};
    auto result = e.addImplMultiplication().getExpression();
    for (const auto& r : result) {
        std::cout << r.getStr() << "\n";
    }

#ifdef DEBUG
    testing::InitGoogleTest();
    return RUN_ALL_TESTS();
#endif
}
