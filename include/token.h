//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKEN_H
#define AST_TOKEN_H

#include <type_traits>
#include <string>
#include <memory>
#include <sstream>
#include <cmath>
#include <stack>
#include <algorithm>
#include <unordered_map>
#include <functional>
#include <utility>
#include <cassert>
#include <complex>

template<typename T>
struct is_complex : std::false_type {};

template<typename T>
struct is_complex<std::complex<T>> : std::true_type {};



template<typename T>
concept FloatingPoint = std::is_floating_point_v<T> || is_complex<T>::value;

class Token {
public:
    enum TokenType : uint8_t {
        ValueType = 0x0,
        UnaryFuncType = 0x1,
        BinaryFuncType = 0x2,
        BracketType = 0x4,
        RightAssociative = 0x08
    };

    explicit Token(std::string val, Token::TokenType t) : value(std::move(val)), type(t) {

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
            type = static_cast<TokenType>(type | TokenType::RightAssociativeBinary);

    };

    Token(const Token& t) = default;

    Token(Token&& t) = default;

    Token& operator=(const Token& t) = default;

    Token& operator=(Token&& t) = default;

    [[nodiscard]] constexpr
    bool isValue() const {
        return type == TokenType::ValueType;
    }

    [[nodiscard]] constexpr
    bool isLiteralValue() const {
        return std::ranges::all_of(value, [](const auto& c){return (c >= '0' && c <= '9') || c == '.';}) && isValue();
    }

    [[nodiscard]] constexpr
    bool isVariableValue() const {
        return std::ranges::all_of(value, [](const auto& c){return std::isalpha(c);}) && isValue();
    }

    [[nodiscard]] constexpr
    bool isValidToken() const {
        return isValue() || isUnaryOp() || isBinaryOp() || isBracket();
    }

    [[nodiscard]] constexpr
    bool isBracket() const {
        return isLeftBracket() || isRightBracket();
    }

    [[nodiscard]] constexpr
    bool isLeftBracket() const {
        return value == "(" && type == TokenType::BracketType;
    }

    [[nodiscard]] constexpr
    bool isRightBracket() const {
        return value == ")" && type == TokenType::BracketType;
    }

    [[nodiscard]] constexpr
    bool isUnaryOp() const {
        return type == TokenType::UnaryFuncType;
    }

    [[nodiscard]] constexpr
    bool isAnyMinus() const {
        return getStr() == "-";
    }

    [[nodiscard]] constexpr
    bool isUnaryMinus() const {
        return isAnyMinus() && isUnaryOp();
    }

    [[nodiscard]] constexpr
    bool isBinaryMinus() const {
        return isAnyMinus() && isBinaryOp();
    }

    [[nodiscard]] constexpr
    bool isBinaryOp() const {
        return type == BinaryFuncType || type == (BinaryFuncType | RightAssociativeBinary);
    }

    [[nodiscard]] constexpr
    bool isRightAssociative() const {
        return (type & TokenType::RightAssociativeBinary);
    }

    [[nodiscard]] constexpr
    const std::string& getStr() const {
        return value;
    }

    constexpr
    friend bool operator==(const Token& lhs, const Token& rhs) {
        return
        lhs.type == rhs.type
        &&
        lhs.value == rhs.value;
    }

    void setAsUnaryMinus() {
        if(value == "-") {
            type = TokenType::UnaryFuncType;
        }
    }

    void setAsBinaryMinus() {
        if (value == "-") {
            type = TokenType::BinaryFuncType;
        }
    }


    template<FloatingPoint T>
    T convert_to () const
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
