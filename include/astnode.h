//
// Created by davidl09 on 10/30/23.
//

#ifndef AST_ASTNODE_H
#define AST_ASTNODE_H

#include "tokenExpr.h"
#include "expression.h"


template<CplxOrRealFloat T>
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
    [[nodiscard]] virtual std::string asString() const = 0;
};



template<CplxOrRealFloat T>
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

    [[nodiscard]] std::string asString() const {
        return std::move(std::to_string(std::real(this->value)));
    }
private:
    T value;
};



template<CplxOrRealFloat T>
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
        return variables.contains(name);
    }

    [[nodiscard]] std::string asString() const {
        return name;
    }
private:
    std::string name;
    std::unordered_map<std::string, T>& variables;
};



template<CplxOrRealFloat T>
class UnaryNode : public AstNode<T> {

public:
    UnaryNode(const std::string& name, const std::unordered_map<std::string_view, std::function<T(T)>>& functions, std::unique_ptr<AstNode<T>>&& child_)
    : self(name)
    {
        if (!functions.contains(name))
            throw std::invalid_argument("Tried initializing unary function node with invalid name");
        eval = functions.at(self);

        child = std::move(child_);
    }

    [[deprecated("Does not initialize 'name' field")]]
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

    [[nodiscard]] std::string asString() const {
        return std::move(std::string{self} + "(" + child->asString() + ")");
    }
private:
    std::string_view self;
    std::function<T(T)> eval;
    std::unique_ptr<AstNode<T>> child;
};



template<CplxOrRealFloat T>
class BinaryNode : public AstNode<T> {
public:
    BinaryNode(const std::string& name, const std::unordered_map<std::string_view, std::function<T(T, T)>>& functions, std::unique_ptr<AstNode<T>>&& leftChild_, std::unique_ptr<AstNode<T>>&& rightChild_)
    : self(name)
    {
        if (!functions.contains(self))
            throw std::invalid_argument("Tried initializing unary function node with invalid name");
        eval = functions.at(self);

        leftChild = std::move(leftChild_);
        rightChild = std::move(rightChild_);
    }

    [[deprecated("Does not initialize 'name' field")]]
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

    [[nodiscard]] std::string asString() const {
        return std::move(leftChild->asString() + std::string{self} + rightChild->asString());
    }
private:
    std::string_view self;
    std::function<T(const T&, const T&)> eval;
    std::unique_ptr<AstNode<T>> leftChild, rightChild;
};



#endif //AST_ASTNODE_H
