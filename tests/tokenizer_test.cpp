//
// Created by davidl09 on 10/3/23.
//

#include <gtest/gtest.h>

#include "tokenizer.h"

TEST(TokenizerTest, seek_paren) {
    std::string test{"(()()(()))"};
    auto a = Tokenizer::seek_closing_paren(test.begin(), test.end());
    EXPECT_EQ(std::distance(static_cast<std::string::const_iterator>(test.begin()), a), 9);

    test = "((())";
    a = Tokenizer::seek_closing_paren(test.begin(), test.end());
    EXPECT_EQ(std::distance(static_cast<std::string::const_iterator>(test.end()), a), 0);

}

TEST(TokenizerTest, check_binary_op) {
    EXPECT_TRUE(Tokenizer::is_unary_op("sin") && !Tokenizer::is_unary_op("cos("));
    EXPECT_TRUE(Tokenizer::is_binary_op("+") && Tokenizer::is_binary_op("-") && Tokenizer::is_binary_op("*") && Tokenizer::is_binary_op("/") && Tokenizer::is_binary_op("^"));
}


TEST(TokenizerTest, token_construct) {
    Token<double> a("3.7");
    EXPECT_TRUE(a.is_value());
    EXPECT_FALSE(a.is_unary_op() || a.is_binary_op());
}
