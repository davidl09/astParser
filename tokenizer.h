//
// Created by davidl09 on 10/3/23.
//

#ifndef AST_TOKENIZER_H
#define AST_TOKENIZER_H

#include <string>
#include <utility>
#include <array>
#include <vector>
#include <algorithm>

template<typename T>
concept FloatingPoint = std::floating_point<T> || std::signed_integral<T>;

namespace Tokenizer {

    constexpr static std::array<std::string, 5> binary_operators{
            "+",
            "-",
            "*",
            "/",
            "^"
    };

    constexpr static std::array<std::string, 18> unary_operators{
            "sqrt",
            "exp",
            "sin",
            "cos",
            "tan",
            "csec",
            "sec",
            "cot",
            "asin",
            "acos",
            "atan",
            "log",
            "log2",
            "ln",
            "real",
            "imag",
            "arg",
            "abs",
    };

    constexpr static bool is_unary_op(const std::string& token) {
        return std::find(unary_operators.begin(), unary_operators.end(), token) != unary_operators.end();
    }

    constexpr static bool is_binary_op(const std::string& token) {
        return std::find(binary_operators.begin(), binary_operators.end(), token) != binary_operators.end();
    }

    constexpr static bool is_binary_op(const char& token) {
        return std::any_of(binary_operators.begin(), binary_operators.end(), [&](const std::string& s){return s[0] == token;});
    }

    constexpr static bool is_r_paren(const char & token) {
        return token == ')';
    }

    constexpr static bool is_l_paren(const char & token) {
        return token == '(';
    }

    constexpr static bool is_any_paren(const char & token) {
        return is_l_paren(token) || is_r_paren(token);
    }

    constexpr static auto seek_closing_paren(std::string::const_iterator open_paren, std::string::const_iterator end) {
        auto paren_count = 0;
        for(; open_paren != end; open_paren++) {
            switch (*open_paren) {
                case '(':
                    ++paren_count;
                    break;
                case ')':
                    --paren_count;
                    break;
            }
            if(paren_count == 0) return open_paren;
        }
        return end;
    }
};



template<FloatingPoint T>
class Token {
public:
    explicit Token(std::string expr) : self(std::move(expr)) {};


    [[nodiscard]] constexpr bool is_value() const{
        return !(Tokenizer::is_unary_op(self) || Tokenizer::is_binary_op(self));
    }

    [[nodiscard]] constexpr bool is_operator() const{
        return !is_value();
    }

    [[nodiscard]] constexpr bool is_binary_op() const{
        return Tokenizer::is_binary_op(self);
    }

    [[nodiscard]] constexpr bool is_unary_op() const{
        return Tokenizer::is_unary_op(self);
    }

private:
    const std::string self;
};

namespace Tokenizer {

    template<FloatingPoint T>
    std::vector<Token<T>> tokenize(const std::string& expression) {
        std::string temp;
        std::vector<Token<T>> result;

        auto handle_unary_minus = [&](std::string::iterator i) {
            if(*i == '-') { //special case: unary minus
                temp.push_back(*i);
                if(!(i == expression.begin() || is_binary_op(*(i-1)))) {
                    result.emplace_back(Token<T>(temp));
                    temp.erase();
                } //if not(is at beginning of expression or previous char is a binary op) then not unary minus

            }
        };

        for (auto i = expression.begin(); i != expression.end(); i++) {
            handle_unary_minus(i);

        }
    }
}


#endif //AST_TOKENIZER_H
