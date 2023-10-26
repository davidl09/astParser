//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKENEXPR_H
#define AST_TOKENEXPR_H

#include "token.h"
#include "tokenizer.h"

class TokenExpression {
public:
    explicit TokenExpression(std::vector<Token> tokens) : expression(std::move(tokens)) {}
    //explicit TokenExpression(const std::vector<Token>& tokens) : expression(tokens) {}

    [[nodiscard]] constexpr
    bool isValidExpression () const {
        for (auto it = expression.begin(); it != expression.end(); ++it) {
            if (it->isBinaryOp()) {
                if (it->getStr() == "-") {
                    //handle unary minus
                    if(it + 1 < expression.end() && (it[1].isUnaryOp() || it[1].isValue()) && (it == expression.begin() || (it - 1)->isLeftBracket() || ))

                }
            }
        }
        return false;
    }

    auto& insertImplicitMult() {

        for (auto it = expression.begin(); it < expression.end() - 1; ++it) {
            if(it->isLiteralValue() && (it[1].isUnaryOp() || it[1].isVariableValue())) {
                auto distance = std::distance(expression.begin(), it);
                expression.insert(it + 1, Token{"*", true, false});
                it = expression.begin() + distance + 1;
            }
        }

        return *this;
    }

    [[nodiscard]] const auto& getPostfixExpression() const {
        return expression;
    }

    [[nodiscard]] const auto& getExpression() const {
        return expression;
    }
private:
    std::vector<Token> expression;
};

#ifdef DEBUG

TEST(tokenExpr, autofillMult) {
    Tokenizer t("3sin(2)");
    TokenExpression e{t.tokenize()};
    auto result = e.insertImplicitMult().getExpression();
    ASSERT_EQ(result[1].getStr(), "*");
}

TEST(tokenExpr, autoFillMult2) {
    Tokenizer t("3x/7");
    TokenExpression e{t.tokenize()};
    auto result = e.insertImplicitMult().getExpression();
    ASSERT_EQ(result[1].getStr(), "*");
}


#endif

#endif //AST_TOKENEXPR_H
