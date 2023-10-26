//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKENIZER_H
#define AST_TOKENIZER_H

#include "token.h"


class Tokenizer {
public:
    Tokenizer(const Tokenizer&) = default;
    Tokenizer(Tokenizer&&) = default;
    
    explicit Tokenizer(std::string_view expr) : expression(expr), current(expression.begin()) {
        if (!matchedBrackets())
            throw std::invalid_argument("Mismatched parentheses");
        else if (!isValidCharExpr())
            throw std::invalid_argument("Invalid Character detected");
    }

    std::vector<Token> tokenize() {
        //any alphanumeric string is treated as a multi-letter variable unless it is followed by a left parenthesis
        std::vector<Token> tokenizedResult;
        
        while (current != expression.end()) {
            
            if (isBracket(current))
                tokenizedResult.emplace_back(handleBracket());

            else if (isFuncCall(current))
                tokenizedResult.emplace_back(handleFuncCall());

            else if (isNumLiteral(current))
                tokenizedResult.emplace_back(handleNumLiteral());
            
            else if (isVariable(current))
                tokenizedResult.emplace_back(handleVariable());
            
            else if (isOperator(current))
                tokenizedResult.emplace_back(handleOperator());

            else ++current;
        }
        return tokenizedResult;
    }

    [[nodiscard]]
    constexpr bool isValidCharExpr() {
        return std::ranges::all_of(expression, [&](const auto& i) -> bool {
            return
                    isOperator(std::string::const_iterator{&i}) ||
                    isBracket(std::string::const_iterator{&i}) ||
                            isValue(std::string::const_iterator{&i}) ||
                    isFuncCall(std::string::const_iterator{&i}) ||
                    i == ' ';
        });
    }

private:

    std::string::const_iterator seekClosingParenthesis(std::string::const_iterator leftParen) {
        std::stack<char> pStack;

        while (leftParen < expression.end()) {
            if (*leftParen == '(')
                pStack.push(*leftParen);
            else if (*leftParen == ')')
                pStack.pop();
            if (pStack.empty()) return leftParen;
            ++leftParen;
        }

        if(leftParen == expression.end())
            throw std::invalid_argument("Expected ')' before end of string");

        return leftParen;
    }

    bool matchedBrackets() {
        int count = 0;
        
        auto it = expression.begin();
        
        while (it != expression.end()) {
            switch (*it) {
                case '(':
                    ++count;
                    break;
                case ')':
                    --count;
                    break;
                default:
                    break;
            }
            ++it;
        }

        return !count;
    }

    constexpr static bool isLeftBracket(std::string::const_iterator c) {
        return *c == '(';
    }

    constexpr static bool isRightBracket(std::string::const_iterator c) {
        return *c == ')';
    }

    constexpr static bool isBracket(std::string::const_iterator c) {
        return isLeftBracket(c) || isRightBracket(c);
    }

    constexpr static bool isNumLiteral(std::string::const_iterator c) {
        return std::isdigit(*c) || *c == '.';
    }

    constexpr bool isVariable(std::string::const_iterator c) {
        return !isFuncCall(c) && std::isalpha(*c);
    }

    constexpr  bool isValue(std::string::const_iterator c) {
        return isVariable(c) || isNumLiteral(c);
    }

    constexpr bool isFuncCall(std::string::const_iterator it) {
        while (it != expression.end() && std::isalpha(*it) && !isLeftBracket(it++));

        if(isLeftBracket(it))
            return true;

        return false;
    }
    
    static constexpr bool isOperator(std::string::const_iterator it) {
        return std::string{"+-*/^"}.find(*it) != std::string::npos;
    }
    
    [[nodiscard]] Token handleNumLiteral()  {
        auto count = 0;
        
        while (current + count < expression.end() && isNumLiteral(current + count)) {
            count++;
        }

        std::string_view result{current, current + count};
        current += count;
        return std::move(Token{std::move(std::string{result}), false, false});
    }

    [[nodiscard]] Token handleVariable() {

        int count = 0;
#ifdef DEBUG
        assert(!this->isFuncCall(current));
#endif

        while(current + count != expression.end() && std::isalpha(current[count])) {
            ++count;
        }

        std::string_view result{current, current + count};
        current += count;
        return std::move(Token{std::move(std::string{result}), false, false});
    }
    
    [[nodiscard]] Token handleFuncCall()  {
        std::string_view result;
        auto begin = current;
        
        while (current != expression.end() && std::isalpha(*current) && !isLeftBracket(current)) {
            result = {begin, ++current};
        }

        return std::move(Token{std::move(std::string{result}), false, true});
    }
    
    [[nodiscard]] Token handleOperator() {
        std::string_view result;
        return std::move(Token{{*current++}, true, false});
    }
    
    [[nodiscard]] Token handleBracket() {
        return std::move(Token{{*current++}, true, true});
    }

    std::string expression;
    std::string::const_iterator current;

};

#ifdef DEBUG

#include <gtest/gtest.h>
TEST(tokenizerTest, ValidOp) {

    //Token(std::string val, bool isBinary, bool isUnary)

    Tokenizer t("3+4");
    std::vector<Token> test{
            Token{"3", false, false},
            Token{"+", true, false},
            Token{"4", false, false}
    };

    auto result = t.tokenize();
    for (auto i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

TEST(tokenizerTest, validFunc) {

    Tokenizer w("sin(a)");
    std::vector<Token> test {
            Token{"sin", false, true},
            Token{"(", true, true},
            Token{"a", false, false},
            Token{")", true, true}
    };

    auto result = w.tokenize();
    for (auto i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

TEST(tokenizerTest, validFuncOp) {

    Tokenizer w("sin(a+2)");
    std::vector<Token> test {
            Token{"sin", false, true},
            Token{"(", true, true},
            Token{"a", false, false},
            Token{"+", true, false},
            Token{"2", false, false},
            Token{")", true, true}
    };

    auto result = w.tokenize();
    for (auto i = 0; i < result.size(); ++i) {
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
            Token{"3", false, false},
            Token{"sin", false, true},
            Token{"(", true, true},
            Token{"a", false, false},
            Token{")", true, true}
    };

    auto result = w.tokenize();
    for (auto i = 0; i < result.size(); ++i) {
        EXPECT_EQ(result[i], test[i]);
    }
}

#endif

#endif //AST_TOKENIZER_H
