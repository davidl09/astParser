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

    auto& addImplMultiplication() {
//adds explicit multiplication operators where they are usually implied by convention
        for (auto it = expression.begin(); it < expression.end() - 1; ++it) {
            if(it->isLiteralValue() && (it[1].isUnaryOp() || it[1].isVariableValue())) {
                auto distance = std::distance(expression.begin(), it);
                expression.insert(it + 1, Token{"*", Token::BinaryFuncType});
                it = expression.begin() + distance + 1;
            }
        }

        return *this;
    }

    auto& setUnaryMinFlags() {
        for (auto it = expression.begin(); it < expression.end() - 1; ++it) {
            if (it->isBinaryMinus()) {
                if(
                        it == expression.begin() // '-' sign at beginning is always unary
                        || (it - 1)->isLeftBracket() //'-' sign after left bracket is always unary
                        || (it - 1)->isBinaryOp() // '-' sign after another operator is always unary
                )
                    it->setAsUnaryMinus();

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
    auto result = e.addImplMultiplication().getExpression();
    ASSERT_EQ(result[1].getStr(), "*");
    ASSERT_TRUE(std::ranges::all_of(result, [](const auto& t){return t.isValidToken();}));
}

TEST(tokenExpr, autoFillMult2) {
    Tokenizer t("3x/7");
    TokenExpression e{t.tokenize()};
    auto result = e.addImplMultiplication().getExpression();
    ASSERT_EQ(result[1].getStr(), "*");
    ASSERT_TRUE(std::ranges::all_of(result, [](const auto& t){return t.isValidToken();}));
}

TEST(tokenExpr, unaryMinus) {
    Tokenizer t("-3x/7");
    TokenExpression e{t.tokenize()};
    auto result = e.addImplMultiplication().setUnaryMinFlags().getExpression();
    ASSERT_TRUE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[2].getStr() == "*");
    ASSERT_TRUE(std::ranges::all_of(result, [](const auto& t){return t.isValidToken();}));
}

TEST(tokenExpr, unaryMinus2) {
    Tokenizer t("3+-x-(-sin(-x))");
    TokenExpression e{t.tokenize()};
    auto result = e.addImplMultiplication().setUnaryMinFlags().getExpression();

    ASSERT_TRUE(result[2].isUnaryMinus());
    ASSERT_TRUE(result[1].isBinaryOp());
    ASSERT_TRUE(result[4].isBinaryMinus());
    ASSERT_TRUE(result[6].isUnaryMinus());
    ASSERT_TRUE(result[7].isUnaryOp());
    ASSERT_FALSE(result[7].isUnaryMinus());
    ASSERT_TRUE(result[9].isUnaryMinus());
    ASSERT_TRUE(result[10].isVariableValue());
    ASSERT_TRUE(std::ranges::all_of(result, [](const auto& t){return t.isValidToken();}));
}

TEST(tokenExpr, unaryMinus3) {
    Tokenizer t{"-3"};
    TokenExpression e{t.tokenize()};
    auto result = e.addImplMultiplication().setUnaryMinFlags().getExpression();

    ASSERT_TRUE(result.size() == 2);
    ASSERT_TRUE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[1].isValue());
    ASSERT_TRUE(result[0].isUnaryOp());
    ASSERT_TRUE(result[1].isLiteralValue());
    ASSERT_FALSE(result[1].isRightAssociatve());
    ASSERT_FALSE(result[0].isRightAssociatve());
    result[0].setAsBinaryMinus();
    ASSERT_FALSE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[0].isBinaryMinus());
    result[0].setAsUnaryMinus();
    ASSERT_TRUE(result.size() == 2);
    ASSERT_TRUE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[1].isValue());
    ASSERT_TRUE(result[0].isUnaryOp());
    ASSERT_TRUE(result[1].isLiteralValue());
    ASSERT_FALSE(result[1].isRightAssociatve());
    ASSERT_FALSE(result[0].isRightAssociatve());
}


#endif

#endif //AST_TOKENEXPR_H
