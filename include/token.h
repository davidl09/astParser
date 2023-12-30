//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKEN_H
#define AST_TOKEN_H

#include <vector>
#include <type_traits>
#include <string>
#include <memory>
#include <sstream>
#include <cmath>
#include <stack>
#include <algorithm>
#include <functional>
#include <utility>
#include <cassert>
#include <complex>

template<typename T>
struct is_complex_floating_point : std::false_type {};

template<typename T>
struct is_complex_floating_point<std::complex<T>> : std::is_floating_point<T> {};

template<typename T>
concept CplxOrRealFloat = std::is_floating_point_v<T> || is_complex_floating_point<T>::value;

class Token {
public:
    enum TokenType : uint8_t {
        ValueType = 1u >> 1,
        UnaryFuncType = 1u,
        BinaryFuncType = 1u << 1,
        BracketType = 1u << 2,
        RightAssociative = 1u << 3
    };

    explicit Token(std::string val, const TokenType t) : value(std::move(val)), type(t) {

        if(!std::ranges::all_of(value, [](const auto& c) -> bool {
            return std::isalnum(c) || std::string_view{"().+-*/^"}.find(c) != std::string::npos;
        })) //if invalid character detected
            throw std::invalid_argument("Tried creating Token with illegal character");

        if (isLiteralValue() && !value.empty()) {
            if(value.front() == '.')
                value.insert(0, 1, '0');
            else if (value.back() == '.')
                value.push_back('0');
        }

        if (value == "^")
            type = static_cast<TokenType>(type | RightAssociative);
    };

    Token(const Token& t) = default;

    Token(Token&& t) = default;

    Token& operator=(const Token& t) = default;

    Token& operator=(Token&& t) = default;

    [[nodiscard]] constexpr
    bool isValue() const {
        return type == ValueType;
    }

    [[nodiscard]]
    bool isLiteralValue() const {
        return std::ranges::all_of(value, [](const auto& c){return std::isdigit(c) || c == '.';}) && isValue();
    }

    [[nodiscard]]
    bool isVariableValue() const {
        return std::ranges::all_of(value, [](const auto& c){return std::isalpha(c);}) && isValue();
    }

    [[nodiscard]]
    bool isValidToken() const {
        return isValue() || isUnaryOp() || isBinaryOp() || isBracket();
    }

    [[nodiscard]]
    bool isBracket() const {
        return isLeftBracket() || isRightBracket();
    }

    [[nodiscard]]
    bool isLeftBracket() const {
        return value.front() == '(' && type == BracketType;
    }

    [[nodiscard]] 
    bool isRightBracket() const {
        return value.front() == ')' && type == BracketType;
    }

    [[nodiscard]] constexpr
    bool isUnaryOp() const {
        return type == UnaryFuncType;
    }

    [[nodiscard]] 
    bool isAnyMinus() const {
        return value.front() == '-';
    }

    [[nodiscard]] 
    bool isUnaryMinus() const {
        return isAnyMinus() && isUnaryOp();
    }

    [[nodiscard]] 
    bool isBinaryMinus() const {
        return isAnyMinus() && isBinaryOp();
    }

    [[nodiscard]] constexpr
    bool isBinaryOp() const {
        return type == BinaryFuncType || type == (BinaryFuncType | RightAssociative);
    }

    [[nodiscard]] constexpr
    bool isRightAssociative() const {
        return (type & RightAssociative);
    }

    [[nodiscard]]
    const std::string& getStr() const {
        return value;
    }


    friend bool operator==(const Token& lhs, const Token& rhs) {
        return
        lhs.type == rhs.type
        &&
        lhs.value == rhs.value;
    }

    void setAsUnaryMinus() {
        if(value == "-") {
            type = UnaryFuncType;
        }
    }

    void setAsBinaryMinus() {
        if (value == "-") {
            type = BinaryFuncType;
        }
    }


    template<CplxOrRealFloat T>
    T convertTo () const
    {
        if(!isValue()) throw std::invalid_argument("Tried converting a non-value to a value");
        std::istringstream ss(value);
        T num;
        ss >> num;
        return num;
    }

private:

    std::string value;
    TokenType type;
};


#endif //AST_TOKEN_H
