//
// Created by davidl09 on 10/23/23.
//

#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include "token.h"
#include "tokenizer.h"
#include "tokenExpr.h"



template<FloatingPoint T>
class AstNode {
public:
    AstNode() = default;
    AstNode(AstNode&&) = default;
    AstNode(const AstNode&) = default;
    virtual ~AstNode() = default;
    [[nodiscard]] virtual T evaluate() const = 0;
};




template<FloatingPoint T>
class ValueNode : public AstNode<T> {

public:
    explicit ValueNode(const Token& token)
    : AstNode<T>(), value(token.convert_to<T>())
    {}

    explicit ValueNode(T value)
    : AstNode<T>(), value(value)
    {}

    ValueNode(ValueNode&& old) noexcept
    : AstNode<T>(), value(std::move(old.value)) {

    }

    [[nodiscard]] T evaluate() const final {
        return value;
    }

private:
    T value;
};

template<FloatingPoint T>
class VariableNode : public AstNode<T> {
public:
    explicit VariableNode(std::string name_, std::unordered_map<std::string, T>& varMap)
    : AstNode<T>(), name(std::move(name_)), variables(varMap)
    {}

    VariableNode(VariableNode&& old) noexcept
    : AstNode<T>(), name(std::move(old.name)), variables(std::move(old.variables))
    {}

    [[nodiscard]] T evaluate() const final {
        return variables.at(name);
    }

private:
    std::string name;
    std::unordered_map<std::string, T>& variables;
};

template<FloatingPoint T>
class UnaryNode : public AstNode<T> {

public:
    UnaryNode(std::function<T(T)> func, std::unique_ptr<AstNode<T>>&& child_)
    : AstNode<T>(), eval(std::move(func)), child(std::move(child_))
    {}
    [[nodiscard]] T evaluate() const final {
        return this->eval(child->evaluate());
    }

    UnaryNode(UnaryNode&& old) noexcept
    : eval(std::move(old.eval)), child(std::move(old.child))
    {}

private:
    std::function<T(T)> eval;
    std::unique_ptr<AstNode<T>> child;
};



template<FloatingPoint T>
class BinaryNode : public AstNode<T> {
public:
    BinaryNode(std::function<T(T,T)> func, std::unique_ptr<AstNode<T>>&& left, std::unique_ptr<AstNode<T>>&& right)
    : AstNode<T>(), eval(func), leftChild(std::move(left)), rightChild(std::move(right))
    {}

    BinaryNode(BinaryNode&& old) noexcept
    : eval(old.func), leftChild(old.leftChild), rightChild(old.rightChild)
    {}

    [[nodiscard]] T evaluate() const final {
        return this->eval(rightChild->evaluate(), leftChild->evaluate());
    }

private:
    std::function<T(const T&, const T&)> eval;
    std::unique_ptr<AstNode<T>> leftChild, rightChild;
};





template<FloatingPoint T>
class Expression {
public:
    explicit Expression(const std::string &expression) : root(nullptr),

    binaryFuncs({
        {"+", [](const T &lhs, const T &rhs) -> T { return lhs + rhs; }},
        {"-", [](const T &lhs, const T &rhs) -> T { return lhs - rhs; }},
        {"*", [](const T &lhs, const T &rhs) -> T { return lhs * rhs; }},
        {"/", [](const T &lhs, const T &rhs) -> T { return lhs / rhs; }},
        {"^", [](const T &lhs, const T &rhs) -> T { return std::pow(lhs, rhs); }},
        /*{"logn", [](const T &base, const T &val) -> T {
                return std::log(val) / std::log(base);
            }},*/
    }),

    unaryFuncs({
        {"sqrt", [](const T &arg) -> T { return static_cast<T>(std::sqrt(arg)); }},
        {"exp",  [](const T &arg) -> T { return static_cast<T>(std::exp(arg)); }},
        {"sin",  [](const T &arg) -> T { return static_cast<T>(std::sin(arg)); }},
        {"cos",  [](const T &arg) -> T { return static_cast<T>(std::cos(arg)); }},
        {"tan",  [](const T &arg) -> T { return static_cast<T>(std::tan(arg)); }},
        {"csec", [](const T &arg) -> T { return static_cast<T>(1) / std::sin(arg); }},
        {"sec",  [](const T &arg) -> T { return static_cast<T>(1) / std::cos(arg); }},
        {"cot",  [](const T &arg) -> T { return static_cast<T>(1) / std::tan(arg); }},
        {"asin", [](const T &arg) -> T { return static_cast<T>(std::asin(arg)); }},
        {"acos", [](const T &arg) -> T { return static_cast<T>(std::acos(arg)); }},
        {"atan", [](const T &arg) -> T { return static_cast<T>(std::atan(arg)); }},
        {"ln",   [](const T &arg) -> T { return static_cast<T>(std::log(arg)); }},
        {"log",  [](const T &arg) -> T { return static_cast<T>(std::log10(arg)); }},
        {"abs",  [](const T &arg) -> T { return static_cast<T>(std::abs(arg)); }},
        {"-",    [](const T &arg) -> T { return -arg; }},
    })
    {
        TokenExpression tokenExpression{Tokenizer(expression).tokenize()};
        auto input = tokenExpression.setUnaryMinFlags().addImplMultiplication().getPostfixExpression();

        std::vector<std::unique_ptr<AstNode<T>>> nodeStack;

        for (auto it = input.begin(); it < input.end(); ++it) {
            if (it->isVariableValue()) {
                nodeStack.emplace_back(
                        std::make_unique<VariableNode<T>>(it->getStr(), variables)
                        );
            }

            else if (it->isLiteralValue()) {
                nodeStack.emplace_back(
                        std::make_unique<ValueNode<T>>(it->convert_to<T>())
                        );
            }

            else if (it->isUnaryOp()) {
                auto temp = std::make_unique<UnaryNode<T>>(unaryFuncs.at(it->getStr()), std::move(nodeStack.back()));
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }

            else if (it->isBinaryOp()) {
                auto temp = std::make_unique<BinaryNode<T>>(binaryFuncs.at(it->getStr()), std::move(nodeStack.rbegin()[0]), std::move(nodeStack.rbegin()[1]));
                nodeStack.pop_back();
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }
        }

        if (nodeStack.size() != 1) {
            std::cout << nodeStack.size();
            throw std::invalid_argument("Unbalanced equation");
        }

        root = std::move(nodeStack.front());
    }

    T evaluate(std::unordered_map<std::string, T> vars) {
        variables = vars;
        return root->evaluate();
    }

    const auto &getBinaryFunc(std::string_view name) {
        return binaryFuncs.at(name);
    }

    const auto &getUnaryFunc(std::string_view name) {
        return unaryFuncs.at(name);
    }

    void addFunction(std::string_view name, std::function<T(T, T)> func) {
        binaryFuncs[name] = func;
    }

    void addFunction(std::string_view name, std::function<T(T)> func) {
        unaryFuncs[name] = func;
    }

    const auto& getUnaryFuncs() {
        return unaryFuncs;
    }

    const auto& getBinaryFuncs() {
        return binaryFuncs;
    }



private:
    std::unique_ptr<AstNode<T>> root;

    std::unordered_map<std::string_view, std::function<T(T,T)>> binaryFuncs;
    std::unordered_map<std::string_view, std::function<T(T)>> unaryFuncs;
    std::unordered_map<std::string, T> variables;
};

#ifdef DEBUG
#include <gtest/gtest.h>

#include <utility>

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

/*
TEST(expression8, invalid_expression) {
    // This test checks if the class handles invalid expressions properly.
    EXPECT_THROW(Expression<double>("2+"), std::runtime_error);
}
*/

TEST(expression9, undefined_variable) {
    // This test checks if the class handles undefined variables properly.
    Expression<double> e("x + y");
    EXPECT_THROW(e.evaluate({{"x", 3.5}}), std::out_of_range);
}

TEST(unaryTest, unaryMinus) {
    Expression<double> e("-5+2");
    EXPECT_DOUBLE_EQ(-3, e.evaluate({{}}));
}

#endif


#endif //AST_EXPRESSION_H
