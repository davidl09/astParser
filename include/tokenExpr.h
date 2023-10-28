//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKENEXPR_H
#define AST_TOKENEXPR_H

#include "token.h"
#include "tokenizer.h"

class TokenExpression {
public:
    explicit TokenExpression(std::vector<Token> tokens, std::unordered_map<std::string, int> order = {{"+", 2},
                                                                                                           {"-", 2},
                                                                                                           {"/", 3},
                                                                                                           {"*", 4},
                                                                                                           {"^", 5}})
    : expression(std::move(tokens)),
    operatorStack(),
    inputStack(),
    evalOrder(std::move(order))
    {
        for (auto it = expression.rbegin(); it < expression.rend(); ++it) {
            //top of inputStack is the beginning of the expression
            inputStack.push(*it);
        }
    }

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

    std::vector<Token> getPostfixExpression() {
        std::vector<Token> output;

        while (!inputStack.empty()) {
            if (inputStack.top().isBracket()) {
                handleBracket(output);
            }

            else if (inputStack.top().isValue()) {
                handleValue(output);
            }

            else if (inputStack.top().isUnaryOp()) {
                handleUnaryOp(output);
            }

            else if (inputStack.top().isBinaryOp()) {
                handleOperator(output);
            }

            else throw std::invalid_argument("Unhandled token");
        }

        while (!operatorStack.empty()) {
            output.emplace_back(operatorStack.top());
            operatorStack.pop();
        }

        return output;
    }

    [[nodiscard]] const auto& getExpression() const {
        return expression;
    }
private:

    auto precedence(const Token& t) const {
        if(t.isBinaryOp())
            return evalOrder.at(t.getStr());

        return 1;
    }

    void handleValue(std::vector<Token>& output) {
        output.emplace_back(inputStack.top());
        inputStack.pop();
    }

    void handleBracket(std::vector<Token>& output) {
        if (inputStack.top().isLeftBracket()) {
            operatorStack.push(inputStack.top());
            inputStack.pop();
        } else if (inputStack.top().isRightBracket()) {
            while (!operatorStack.empty() && !operatorStack.top().isLeftBracket()) {
                output.emplace_back(operatorStack.top());
                operatorStack.pop();
            }
            assert(operatorStack.top().getStr() == "(");
            operatorStack.pop();
            inputStack.pop();
            //error if the popped value is not '('
        }
    }

    void handleOperator(std::vector<Token>& output) {
        while (
                !(operatorStack.empty() || operatorStack.top().isLeftBracket())
                && (
                        (precedence(operatorStack.top()) > precedence(inputStack.top())
                        ||
                        (precedence(operatorStack.top()) >= precedence(inputStack.top()) && !inputStack.top().isRightAssociative()))
                    )
                )
        {
            output.emplace_back(operatorStack.top());
            operatorStack.pop();
        }
        operatorStack.push(inputStack.top());
        inputStack.pop();
    }

    void handleUnaryOp(std::vector<Token>& output) {
        if (inputStack.top().isUnaryOp()) {
            operatorStack.push(inputStack.top());
            inputStack.pop();
        }
#ifdef DEBUG
        else throw std::invalid_argument{"Called unary func handler on invalid token type"};
#endif

    }


    std::vector<Token> expression;

    std::stack<Token, std::vector<Token>> operatorStack;
    std::stack<Token, std::vector<Token>> inputStack;

    std::unordered_map<std::string, int> evalOrder;
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


#endif

#endif //AST_TOKENEXPR_H
