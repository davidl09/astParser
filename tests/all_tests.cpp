//
// Created by davidl09 on 10/29/23.
//

#include "expression.h"


#include <gtest/gtest.h>

TEST(tokenTest, varTest) {
    Token t{"a", Token::ValueType};
    ASSERT_TRUE(t.isValue());
    ASSERT_TRUE(t.isVariableValue());
    ASSERT_FALSE(t.isLiteralValue());
    ASSERT_TRUE(t.isValidToken());
}

TEST(tokenTest, constructTest) {
    Token t{"(", Token::BracketType};
    ASSERT_TRUE(t.isBracket());
    ASSERT_TRUE(t.isLeftBracket());
    ASSERT_FALSE(t.isLiteralValue());
    ASSERT_FALSE(t.isUnaryOp());
    ASSERT_FALSE(t.isBinaryOp());
}

TEST(tokenTest, funcTest) {
    Token t{"sin", Token::UnaryFuncType};
    ASSERT_TRUE(t.isUnaryOp());
    ASSERT_FALSE(t.isUnaryMinus());
    ASSERT_FALSE(t.isAnyMinus());
    ASSERT_FALSE(t.isVariableValue());
    ASSERT_FALSE(t.isBinaryOp());
    ASSERT_TRUE(t.isUnaryOp());
}

TEST(tokenTest, rAssociateTest) {
    Token t{"^", Token::BinaryFuncType};
    ASSERT_TRUE(t.isBinaryOp());
    ASSERT_TRUE(t.isRightAssociative());
    ASSERT_FALSE(t.isUnaryOp());
    ASSERT_FALSE(t.isVariableValue());
}

TEST(tokenTest, bracketTest) {
    Token t{")", Token::BracketType};
    ASSERT_TRUE(t.isBracket());
    ASSERT_TRUE(t.isRightBracket());
    ASSERT_FALSE(t.isLeftBracket());
    ASSERT_FALSE(t.isLiteralValue());
    ASSERT_FALSE(t.isUnaryOp());
    ASSERT_FALSE(t.isBinaryOp());
}


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
    ASSERT_FALSE(result[1].isRightAssociative());
    ASSERT_FALSE(result[0].isRightAssociative());
    result[0].setAsBinaryMinus();
    ASSERT_FALSE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[0].isBinaryMinus());
    result[0].setAsUnaryMinus();
    ASSERT_TRUE(result.size() == 2);
    ASSERT_TRUE(result[0].isUnaryMinus());
    ASSERT_TRUE(result[1].isValue());
    ASSERT_TRUE(result[0].isUnaryOp());
    ASSERT_TRUE(result[1].isLiteralValue());
    ASSERT_FALSE(result[1].isRightAssociative());
    ASSERT_FALSE(result[0].isRightAssociative());
}



TEST(tokenExpr, postFix) {
    TokenExpression t{Tokenizer("3+2").tokenize()};
    auto result = t.addImplMultiplication().setUnaryMinFlags().getPostfixExpression();

    ASSERT_TRUE(result[0].getStr() == "3");
    ASSERT_TRUE(result[1].getStr() == "2");
    ASSERT_TRUE(result[2].getStr() == "+");
}

TEST(tokenExpr, postFix2) {
    TokenExpression t{Tokenizer("sin(3+2)").tokenize()};
    auto result = t.addImplMultiplication().setUnaryMinFlags().getPostfixExpression();

    ASSERT_TRUE(result[0].getStr() == "3");
    ASSERT_TRUE(result[1].getStr() == "2");
    ASSERT_TRUE(result[2].getStr() == "+");
    ASSERT_TRUE(result[3].getStr() == "sin");
}

TEST(tokenExpr, postFix3) {
    TokenExpression t{Tokenizer("3^sin(2+exp(3/5+1))").tokenize()};
    auto result = t.addImplMultiplication().setUnaryMinFlags().getPostfixExpression();

    auto expResult = "3235/1+exp+sin^";

    std::string testResult;

    for (auto& token : result) {
        testResult += token.getStr();
    }

    ASSERT_STREQ(testResult.c_str(), expResult);
}

TEST(exprUsage, unaryMinus) {

    TokenExpression tokenExpression{Tokenizer{"-5+2"}.tokenize()};
    auto input = tokenExpression.setUnaryMinFlags().addImplMultiplication().getPostfixExpression();

    ASSERT_TRUE(input.end() != std::find_if(input.begin(), input.end(), [](const auto& i){return i.isUnaryMinus();}));

}


TEST(tokenizerTest, ValidOp) {

    Tokenizer t("3+4");
    std::vector<Token> test{
            Token{"3", Token::ValueType},
            Token{"+", Token::BinaryFuncType},
            Token{"4", Token::ValueType}
    };

    auto result = t.tokenize();
    for (size_t i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

TEST(tokenizerTest, validFunc) {

    Tokenizer w("sin(a)");
    std::vector<Token> test {
            Token{"sin", Token::UnaryFuncType},
            Token{"(", Token::BracketType},
            Token{"a", Token::ValueType},
            Token{")", Token::BracketType}
    };

    auto result = w.tokenize();
    for (size_t i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

TEST(tokenizerTest, validFuncOp) {

    Tokenizer w("sin(a+2)");
    std::vector<Token> test {
            Token{"sin", Token::UnaryFuncType},
            Token{"(", Token::BracketType},
            Token{"a", Token::ValueType},
            Token{"+", Token::BinaryFuncType},
            Token{"2", Token::ValueType},
            Token{")", Token::BracketType}
    };

    auto result = w.tokenize();
    for (size_t i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

TEST(tokenizerTest, invalidChar) {
    EXPECT_THROW(Tokenizer t("sin(a^&3)"); t.tokenize(), std::invalid_argument);
}

TEST(tokenizerTest, mismatchParen) {
    EXPECT_THROW(Tokenizer t("sin(3x"); t.tokenize(), std::invalid_argument);
}

TEST(tokenizerTest, implicitMult) {
    Tokenizer w("3sin(a)");
    std::vector<Token> test {
            Token{"3", Token::ValueType},
            Token{"sin", Token::UnaryFuncType},
            Token{"(", Token::BracketType},
            Token{"a", Token::ValueType},
            Token{")", Token::BracketType}
    };

    auto result = w.tokenize();
    for (size_t i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}


TEST(expression, add) {
    Expression<double> e("3+2");
    EXPECT_DOUBLE_EQ(e.evaluate({}), 5);
}

TEST(expression1, funcs) {
    Expression<double> e("sin(1+x)");
    EXPECT_DOUBLE_EQ(0, e.evaluate({{"x", -1}}));
    EXPECT_DOUBLE_EQ(std::sin(1), e.evaluate({{"x", 0}}));
}


TEST(expression2, subtract) {
    Expression<double> e("5-3");
    EXPECT_DOUBLE_EQ(e.evaluate({}), 2);
}

TEST(expression3, multiply) {
    Expression<double> e("4*3");
    EXPECT_DOUBLE_EQ(e.evaluate({}), 12);
}

TEST(expression4, divide) {
    Expression<double> e("10/2");
    EXPECT_DOUBLE_EQ(e.evaluate({}), 5);
}

TEST(expression5, complex) {
    Expression<double> e("3+2*4-6/2");
    EXPECT_DOUBLE_EQ(e.evaluate({}), 8);
}

TEST(expression6, funcs) {
    Expression<double> e("sin(1+x)");
    EXPECT_DOUBLE_EQ(0, e.evaluate({{"x", -1}}));
    EXPECT_DOUBLE_EQ(std::sin(1), e.evaluate({{"x", 0}}));
}

TEST(expression7, variables) {
    Expression<double> e("x + y");
    EXPECT_DOUBLE_EQ(e.evaluate({{"x", 3.5}, {"y", 2.5}}), 6.0);
}


TEST(expression8, invalid_expression) {
    // This test checks if the class handles invalid expressions properly.
    EXPECT_THROW(Expression<double>("2+"), std::runtime_error);
}


TEST(expression9, undefined_variable) {
    // This test checks if the class handles undefined variables properly.
    Expression<double> e("x + y");
    EXPECT_THROW(e.evaluate({{"x", 3.5}}), std::out_of_range);
}

TEST(unaryTest, unaryMinus) {
    Expression<double> e("-5+2");
    EXPECT_DOUBLE_EQ(-3, e.evaluate({{}}));
}

TEST(expression0, implicitMult) {
    Expression<double> e("3(4+6)/17");
    ASSERT_TRUE(e.evaluate({{}}) == (3 * 10.0 / 17));
}

TEST(expressionTest, cloneTest) {
    Expression<double> e{"3+4(5-6)/5+7/15"};
    auto res1 = e.evaluate({{}});
    Expression<double> copy(e);
    auto res2 = e.evaluate({{}});
    EXPECT_DOUBLE_EQ(res1, res2);
}


int main() {
    ::testing::InitGoogleTest();
    return RUN_ALL_TESTS();
}