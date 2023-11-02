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

        std::erase_if(expression, [](const auto& c){return c <= ' ';});
        
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

            else throw std::invalid_argument("Invalid charater detected: " + std::string{*current});
        }
        return tokenizedResult;
    }

    [[nodiscard]]
    bool isValidCharExpr() {
        return std::all_of(expression.begin(), expression.end(), [&](const auto& i) -> bool {
            auto index = std::string::const_iterator{expression.begin()} + (&i - &expression[0]);
            return
                    isOperator(index) ||
                    isBracket(index) ||
                    isValue(index) ||
                    isFuncCall(index) ||
                    i == ' ';
        });
    }

private:
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

    static bool isLeftBracket(std::string::const_iterator c) {
        return *c == '(';
    }

    static bool isRightBracket(std::string::const_iterator c) {
        return *c == ')';
    }

    static bool isBracket(std::string::const_iterator c) {
        return isLeftBracket(c) || isRightBracket(c);
    }

    static bool isNumLiteral(std::string::const_iterator c) {
        return std::isdigit(*c) || *c == '.';
    }

    bool isVariable(std::string::const_iterator c) {
        return !isFuncCall(c) && std::isalpha(*c);
    }

     bool isValue(const std::string::const_iterator c) {
        return isVariable(c) || isNumLiteral(c);
    }

    bool isFuncCall(std::string::const_iterator it) {
        while (it != expression.end() && std::isalpha(*it) && !isLeftBracket(it++));

        if(isLeftBracket(it))
            return true;

        return false;
    }
    
    static bool isOperator(std::string::const_iterator it) {
        return std::string{"+-*/^"}.find(*it) != std::string::npos;
    }
    
    [[nodiscard]] Token handleNumLiteral()  {
        auto count = 0;
        
        while (current + count < expression.end() && isNumLiteral(current + count)) {
            count++;
        }

        std::string_view result{current, current + count};
        current += count;
        return Token{std::string{result}, Token::ValueType};
    }

    [[nodiscard]] Token handleVariable() {

        int count = 0;

        while(current + count != expression.end() && std::isalpha(current[count])) {
            ++count;
        }

        std::string_view result{current, current + count};
        current += count;
        return Token{std::string{result}, Token::ValueType};
    }
    
    [[nodiscard]] Token handleFuncCall()  {
        std::string_view result;
        auto begin = current;
        
        while (current != expression.end() && std::isalpha(*current) && !isLeftBracket(current)) {
            result = {begin, ++current};
        }

        return Token{std::string{result}, Token::UnaryFuncType};
    }
    
    [[nodiscard]] Token handleOperator() {
        std::string_view result;
        return Token{{*current++}, Token::BinaryFuncType};
    }
    
    [[nodiscard]] Token handleBracket() {
        return Token{{*current++}, Token::BracketType};
    }

    std::string expression;
    std::string::const_iterator current;

};

#endif //AST_TOKENIZER_H
