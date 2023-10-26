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

#ifdef DEBUG
#include <gtest/gtest.h>
#endif


template<typename T>
concept FloatingPoint = std::is_floating_point_v<T>;

class Token {
public:
    explicit Token(std::string val, bool isBinary, bool isUnary) : value(std::move(val)), isBinaryOperator(isBinary), isUnaryOperator(isUnary) {

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

    };

    Token(const Token& t) {
        value = t.value;
        isUnaryOperator = t.isUnaryOperator;
        isBinaryOperator = t.isBinaryOperator;
    }

    Token(Token&& t) noexcept {
        value = std::move(t.value);
        isUnaryOperator = t.isUnaryOperator;
        isBinaryOperator = t.isBinaryOperator;
    }

    Token& operator=(const Token& t) = default;

    Token& operator=(Token&& t) noexcept {
        value = std::move(t.value);
        isUnaryOperator = t.isUnaryOperator;
        isBinaryOperator = t.isBinaryOperator;
        return *this;
    }


    [[nodiscard]] constexpr
    bool isValue() const {
        return !(isBinaryOperator || isUnaryOperator);
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
        return value == "(" && isBinaryOperator && isUnaryOperator;
    }

    [[nodiscard]] constexpr
    bool isRightBracket() const {
        return value == ")" && isBinaryOperator && isUnaryOperator;
    }

    [[nodiscard]] constexpr
    bool isUnaryOp() const {
        return isUnaryOperator && !isBinaryOperator;
    }

    [[nodiscard]] constexpr
    bool isUnaryMinus() const {
        return getStr() == "-" && isUnaryOp();
    }

    [[nodiscard]] constexpr
    bool isBinaryOp() const {
        return isBinaryOperator && !isUnaryOperator;
    }

    [[nodiscard]] constexpr
    const std::string& getStr() const {
        return value;
    }

    constexpr
    friend bool operator==(const Token& lhs, const Token& rhs) {
        return
        lhs.isUnaryOperator == rhs.isUnaryOperator
        &&
        lhs.isBinaryOperator == rhs.isBinaryOperator
        &&
        lhs.value == rhs.value;
    }

private:

    template<FloatingPoint T>
    T convert_to () const
    {
        std::istringstream ss(value);
        T num;
        ss >> num;
        return std::move(num);
    }

    std::string value;
    bool isBinaryOperator;
    bool isUnaryOperator;
};

#ifdef DEBUG

TEST(tokenTest, varTest) {
    Token t{"a", false, false};
    ASSERT_TRUE(t.isValue());
    ASSERT_TRUE(t.isVariableValue());
    ASSERT_FALSE(t.isLiteralValue());
    ASSERT_TRUE(t.isValidToken());
}

TEST(tokenTest, constructTest) {
    Token t{"(", true, true};
    ASSERT_TRUE(t.isBracket());
    ASSERT_TRUE(t.isLeftBracket());
    ASSERT_FALSE(t.isLiteralValue());
    ASSERT_FALSE(t.isUnaryOp());
    ASSERT_FALSE(t.isBinaryOp());
}

#endif


#endif //AST_TOKEN_H
