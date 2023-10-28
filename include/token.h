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
    enum TokenType : uint8_t {
        ValueType = 0x0,
        UnaryFuncType = 0x1,
        BinaryFuncType = 0x2,
        BracketType = 0x3,
        RightAssociativeBinary = 0x2 | 0x4,
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

    constexpr void setAsUnaryMinus() {
        if(value == "-") {
            type = TokenType::UnaryFuncType;
        }
    }

    constexpr void setAsBinaryMinus() {
        if (value == "-") {
            type = TokenType::BinaryFuncType;
        }
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
    TokenType type;
};

#ifdef DEBUG

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

#endif


#endif //AST_TOKEN_H
