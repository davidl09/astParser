//
// Created by davidl09 on 10/23/23.
//

#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include "token.h"
#include "tokenizer.h"
#include "tokenExpr.h"

#include <stdexcept>
#include <iostream>



template<FloatingPoint T>
class AstNode {
public:
    AstNode() = default;
    AstNode(AstNode&&) = default;
    AstNode(const AstNode&) = default;
    virtual ~AstNode() = default;

    [[nodiscard]] virtual T evaluate() const = 0;
    [[nodiscard]] virtual T evalThreadSafe(const std::unordered_map<std::string, T>& map) const = 0;
    [[nodiscard]] virtual std::unique_ptr<AstNode<T>> clone() const = 0;
    [[nodiscard]] virtual bool validateNode() const = 0;
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
    : AstNode<T>(), value(std::move(old.value))
    {}

    ValueNode(const ValueNode& old)
    : value(old.value)
    {}

    [[nodiscard]] std::unique_ptr<AstNode<T>> clone() const final {
        return std::move(std::make_unique<ValueNode<T>>(this->value));
    }

    [[nodiscard]] T evaluate() const final {
        return value;
    }

    [[nodiscard]] T evalThreadSafe(const std::unordered_map<std::string, T>& map) const final {
        return value;
    }

    [[nodiscard]] bool validateNode() const {
        return true;
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

    VariableNode(const VariableNode& old)
    : name(old.name), variables(old.variables)
    {}

    [[nodiscard]] std::unique_ptr<AstNode<T>> clone() const final {
        return std::move(std::make_unique<VariableNode<T>>(name, variables));
    }

    [[nodiscard]] T evaluate() const final {
        return variables.at(name);
    }

    [[nodiscard]] T evalThreadSafe(const std::unordered_map<std::string, T>& map) const final {
        return map.at(name);
    }

    [[nodiscard]] bool validateNode() const {
        try {
            variables.at(name);
        }
        catch (std::out_of_range& e) {
            std::cerr << "Missing variable value: " << name << "\n";
            return false;
        }
        return true;
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

    UnaryNode(UnaryNode&& old) noexcept
    : eval(std::move(old.eval)), child(std::move(old.child))
    {}

    UnaryNode(const UnaryNode& old) {
        *this = old.clone();
    }

    [[nodiscard]] std::unique_ptr<AstNode<T>> clone() const final {
        return std::move(std::make_unique<UnaryNode<T>>(std::function<T(T)>(eval), child->clone()));
    }

    [[nodiscard]] T evaluate() const final {
        return this->eval(child->evaluate());
    }

    [[nodiscard]] T evalThreadSafe(const std::unordered_map<std::string, T>& map) const final {
        return this->eval(child->evalThreadSafe(map));
    }

    [[nodiscard]] bool validateNode() const {
        return child->validateNode();
    }
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

    BinaryNode(const BinaryNode& old) {
        *this = old.clone();
    }

    [[nodiscard]] std::unique_ptr<AstNode<T>> clone() const final {
        return std::move(std::make_unique<BinaryNode<T>>(std::function<T(T,T)>{eval}, std::move(leftChild->clone()), std::move(rightChild->clone())));
    }

    [[nodiscard]] T evaluate() const final {
        return this->eval(leftChild->evaluate(), rightChild->evaluate());
    }

    [[nodiscard]] T evalThreadSafe(const std::unordered_map<std::string, T>& map) const final {
        return this->eval(leftChild->evalThreadSafe(map), rightChild->evalThreadSafe(map));
    }

    [[nodiscard]] bool validateNode() const {
        return leftChild->validateNode() && rightChild->validateNode();
    }
private:
    std::function<T(const T&, const T&)> eval;
    std::unique_ptr<AstNode<T>> leftChild, rightChild;
};



template<FloatingPoint T>
class Expression {
public:
    explicit Expression(const std::string &expression)
    : isValid(false), root(nullptr),

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
        checkAndInit(expression);
    }

    Expression(const Expression& old)
    : isValid(old.isValid), root(old.root->clone()), binaryFuncs(old.binaryFuncs), unaryFuncs(old.unaryFuncs), variables(old.variables)
    {}

    Expression(Expression&& old) noexcept
    : isValid(old.isValid), root(std::move(old.root)), binaryFuncs(std::move(old.binaryFuncs)), unaryFuncs(std::move(old.unaryFuncs)), variables(std::move(old.variables))
    {
        old.isValid = false;
    }

    Expression()
    : isValid(false), root(nullptr)
    {}

    T evaluate(const std::unordered_map<std::string, T>& vars) {
        variables = vars;
        if (!isValid) throw std::invalid_argument("Tried to evaluate invalid expression");
        return root->evaluate();
    }

    [[nodiscard]] bool isValidExpr() const {
        return isValid;
    }

    const auto &getBinaryFunc(std::string_view name) const {
        return binaryFuncs.at(name);
    }

    const auto &getUnaryFunc(std::string_view name) const {
        return unaryFuncs.at(name);
    }

    const auto& getVariables() const {
        return variables;
    }

    void addFunction(std::string_view name, std::function<T(T, T)> func) {
        binaryFuncs[name] = func;
    }

    void addFunction(std::string_view name, std::function<T(T)> func) {
        unaryFuncs[name] = func;
    }

    const auto& getUnaryFuncs() const {
        return unaryFuncs;
    }

    const auto& getBinaryFuncs() const {
        return binaryFuncs;
    }


    friend std::istream& operator>>(std::istream& in, Expression<T>& e) {
        std::string input;
        std::getline(std::cin, input);
        e.checkAndInit(input);
        return in;
    }

    void checkAndInit(const std::string& expression, std::unordered_map<std::string, T> vars = {{}}) {
        try {
            init(expression);
        }
        catch (std::invalid_argument& e) {
            std::cerr << "Invalid Expression: " << e.what() << "\n";
            invalidate();
            return;
        }
        isValid = root->validateNode();

        if (!isValid) {
            invalidate();
        }
    }

private:

    void init(const std::string& expression) {
        Tokenizer tokenizer(expression);
        if (!tokenizer.isValidCharExpr()) throw std::invalid_argument("Invalid expression");

        TokenExpression tokenExpression{tokenizer.tokenize()};
        auto tempVars = tokenExpression.getVariables();
        std::ranges::for_each(tempVars, [&](const Token& t){variables[t.getStr()];});

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
                auto temp = std::make_unique<BinaryNode<T>>(binaryFuncs.at(it->getStr()), std::move(nodeStack.rbegin()[1]), std::move(nodeStack.rbegin()[0]));
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

        if (root == nullptr) {
            throw std::invalid_argument("Malformed expression, could not generate parse tree");
        }
    }

    void invalidate() {
        isValid = false;
        root.reset(nullptr);
    }

    bool isValid;

    std::unique_ptr<AstNode<T>> root;

    std::unordered_map<std::string_view, std::function<T(T,T)>> binaryFuncs;
    std::unordered_map<std::string_view, std::function<T(T)>> unaryFuncs;
    std::unordered_map<std::string, T> variables;
};

#endif //AST_EXPRESSION_H
