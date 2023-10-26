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
    AstNode(const std::string&) = delete;
    virtual ~AstNode() = default;
    virtual const T& get() = 0;
};




template<FloatingPoint T>
class ValueNode : public AstNode<T> {

public:
    explicit ValueNode(const T& value) : value(value) {}
    explicit ValueNode(const std::string& in) : value(convert_to<T>(in)) {}

    const T& get() const {
        return value;
    }

private:
    T value;
};




template<FloatingPoint T>
class UnaryNode : public AstNode<T> {

public:
    /*UnaryNode(const std::function<T(const T&)>& function) : AstNode<T>(), eval(function) {}
    UnaryNode(const std::string& in) : AstNode<T>(), eval(Parsing::unary_funcs<T>.at(in)) {}*/

    const T& get() const {
        return this->eval(child->get());
    }

private:
    std::function<T(T)> eval;
    std::unique_ptr<AstNode<T>> child;
};





template<FloatingPoint T>
class BinaryNode : public AstNode<T> {

public:
    const T& get() const {
        return this->eval(leftChild->get(), rightChild->get());
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
    {}


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
};


#endif //AST_EXPRESSION_H
