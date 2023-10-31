//
// Created by davidl09 on 10/30/23.
//

#ifndef AST_ASTNODE_H
#define AST_ASTNODE_H

#include "tokenExpr.h"


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
        catch (std::exception& e) {
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



#endif //AST_ASTNODE_H
