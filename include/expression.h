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
    explicit VariableNode(std::string name_, const std::unordered_map<std::string, T>& varMap)
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
    std::unordered_map<std::string, T> variables;
};

template<FloatingPoint T>
class UnaryNode : public AstNode<T> {

public:
    UnaryNode(std::function<T(T)> func, std::unique_ptr<AstNode<T>> child_)
    : AstNode<T>(), eval(func), child(child_)
    {}
    [[nodiscard]]T evaluate() const {
        return this->eval(child->evaluate());
    }

    UnaryNode(UnaryNode&& old)
    : eval(old.eval), child(old.child)
    {}

private:
    std::function<T(T)> eval;
    std::unique_ptr<AstNode<T>> child;
};



template<FloatingPoint T>
class BinaryNode : public AstNode<T> {
public:
    BinaryNode(std::function<T(T,T)> func, std::unique_ptr<AstNode<T>> left, std::unique_ptr<AstNode<T>> right)
    : AstNode<T>(), eval(func), leftChild(std::move(left)), rightChild(std::move(right))
    {}

    BinaryNode(BinaryNode&& old) noexcept
    : eval(old.func), leftChild(old.leftChild), rightChild(old.rightChild)
    {}

    [[nodiscard]] T evaluate() const final {
        return this->eval(leftChild->evaluate(), rightChild->evaluate());
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

        /*std::vector<std::unique_ptr<AstNode<T>>> nodeStack;

        for (auto it = input.begin(); it < input.end(); ++it) {
            if (it->isLiteralValue()) {
                nodeStack.emplace_back(std::make_unique<ValueNode<T>>(*it));
            }

            else if (it->isVariableValue()) {
                nodeStack.emplace_back(std::make_unique<VariableNode<T>>(it->getStr(), variables));
            }

            else if (it->isUnaryOp()) {
                auto temp = std::make_unique<UnaryNode<T>>(unaryFuncs.at(it->getStr()), std::move(expression.back()));
                nodeStack.pop_back();
                nodeStack.emplace_back(temp);
            }

            else if (it->isBinaryOp()) {
                auto temp = std::make_unique<BinaryNode<T>>(binaryFuncs.at(it->getStr()), nodeStack.rbegin()[0].get(), nodeStack.rbegin()[1].get());
                nodeStack.pop_back();
                nodeStack.pop_back();
                nodeStack.emplace_back(temp);
            }
        }

        if (nodeStack.size() != 1) throw std::invalid_argument("Parser error");*/



    }

    T evaluate(std::unordered_map<std::string, T> vars) {
        variables = vars;

        return root->get();
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
}

#endif


#endif //AST_EXPRESSION_H
