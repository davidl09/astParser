//
// Created by davidl09 on 10/23/23.
//

#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include <stdexcept>
#include <iostream>

#include "tokenExpr.h"

#include "gamma.h"
#include "mandelbrot.h"

template<CplxOrRealFloat T>
class Expression {
private:
    class AstNode;
public:
    explicit Expression(const std::string &expression, bool noExcept = true)
    : isValid(false), root(nullptr),

    binaryFuncs({
        {"+", [](const T &lhs, const T &rhs) -> T { return lhs + rhs; }},
        {"-", [](const T &lhs, const T &rhs) -> T { return lhs - rhs; }},
        {"*", [](const T &lhs, const T &rhs) -> T { return lhs * rhs; }},
        {"/", [](const T &lhs, const T &rhs) -> T { return lhs / rhs; }},
        {"^", [](const T &lhs, const T &rhs) -> T { return std::pow(lhs, rhs); }},
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
        if constexpr (is_complex_floating_point<T>::value) {
            unaryFuncs["mandelbrot"] = mandelbrot<T>;
            unaryFuncs["arg"] = [](const T &arg) -> T {return std::arg(arg);};
            unaryFuncs["real"] = [](const T &arg) -> T {return std::real(arg);};
            unaryFuncs["imag"] = [](const T &arg) -> T {return std::imag(arg);};
            unaryFuncs["gamma"] = gamma_complex<T>;
        }

        if (noExcept) {
            checkAndInit(expression);
        }
        else {
            checkInitWithExcept(expression);
        }
    }

    Expression(const Expression& old)
    : isValid(old.isValid), root(old.root ? old.root->clone() : nullptr), binaryFuncs(old.binaryFuncs), unaryFuncs(old.unaryFuncs), variables(old.variables)
    {}

    Expression(Expression&& old) noexcept
    : isValid(old.isValid), root(old.root ? std::move(old.root) : nullptr), binaryFuncs(std::move(old.binaryFuncs)), unaryFuncs(std::move(old.unaryFuncs)), variables(std::move(old.variables))
    {}

    Expression() {
        *this = Expression<T>("0");
        isValid = true;
    }

    T evaluate(const std::unordered_map<std::string, T>& vars) const {

        if (!isValid) throw std::invalid_argument("Tried to evaluate invalid expression");
        return root->evalThreadSafe(vars);

    }

    T evaluate(const std::unordered_map<std::string, T>& vars) {
        //insert provided variables into expression's var object
        try {
            std::for_each(variables.begin(), variables.end(), [&](auto& keyVal) {
                try {
                    keyVal.second = vars.at(keyVal.first);
                }
                catch(std::out_of_range& r) {
                    throw std::invalid_argument(keyVal.first);
                }
            });
        }
        catch (std::exception& e) {
            throw std::invalid_argument(std::string{"Unspecified variable value: "} + e.what());
            //each variable in the expression must have a value provided to it by the map
        }


        if (!isValid) throw std::invalid_argument("Tried to evaluate invalid expression");
        return root->evaluate();
    }

    T evaluate() const {
        return evaluate({{}});
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

    void addFunction(const std::string& name, std::function<T(T, T)> func) {
        binaryFuncs[name] = func;
    }

    void addFunction(const std::string& name, std::function<T(T)> func) {
        unaryFuncs[name] = func;
    }

    const auto& getUnaryFuncs() const {
        return unaryFuncs;
    }

    const auto& getBinaryFuncs() const {
        return binaryFuncs;
    }

#ifndef BUILD_PYMODULE
    friend std::istream& operator>>(std::istream& in, Expression<T>& e) {
        std::string input;
        std::getline(std::cin, input);
        e.checkInitWithExcept(input);
        return in;
    }
#endif

    Expression& operator=(const Expression& rhs) {
        isValid = rhs.isValid;
        root = rhs.root ? rhs.root->clone() : nullptr;
        binaryFuncs = rhs.binaryFuncs;
        unaryFuncs = rhs.unaryFuncs;
        return *this;
    }

    void checkAndInit(const std::string& expression) noexcept {
        //checks validity of expression and catches exceptions -> return invalidated (unusable expression) if something goes wrong

        try {
            init(expression);
        }
        catch (std::invalid_argument& e) {
            invalidate();
            return;
        }
        if (!root) {
            invalidate();
        }

        isValid = root->validateNode();

        if (!isValid) {
            invalidate();
        }
    }


    void checkInitWithExcept(const std::string& expression) {
        init(expression);
        isValid = root->validateNode();
    }

    auto asExpressionLambda() const {
        if (!isValid) throw std::runtime_error("Cannot create function from invalid expression");
        if (!variables.empty()) throw std::runtime_error("Cannot create 0-variable expression from variable function");

        return [*this](const std::unordered_map<std::string, T>& vars = {{}}){
            Expression<T> copy(*this);
            return copy.evaluate(vars);
        };
    }

    const std::string& string() {
        return self;
    }

private:

    void init(const std::string& expression) {
        if (expression.empty()) {
            throw std::invalid_argument("Cannot initialize empty expression");
        }

        self = expression;

        Tokenizer tokenizer(expression);
        if (!tokenizer.isValidCharExpr()) throw std::invalid_argument("Invalid expression");
        TokenExpression tokenExpression{tokenizer.tokenize()};

        auto tempVars = tokenExpression.getVariables();
        variables.clear();

        std::for_each(tempVars.begin(), tempVars.end(), [&](const Token& t){variables[t.getStr()];});

        auto postfixExpression = tokenExpression.setUnaryMinFlags().addImplMultiplication().getPostfixExpression();
        std::vector<std::unique_ptr<AstNode>> nodeStack;


        for (auto it = postfixExpression.begin(); it < postfixExpression.end(); ++it) {
            if (it->isVariableValue()) {
                nodeStack.emplace_back(
                        std::make_unique<VariableNode>(it->getStr(), variables)
                        );
            }

            else if (it->isLiteralValue()) {
                nodeStack.emplace_back(
                        std::make_unique<ValueNode>(it->convert_to<T>())
                        );
            }

            else if (it->isUnaryOp()) {
                if (nodeStack.empty()) {
                    throw std::invalid_argument("Expected argument to unary operator '" + it->getStr() + "'");
                }

                if (!unaryFuncs.contains(it->getStr())) {
                    throw std::invalid_argument("Unknown function encountered: " + it->getStr());
                }
                auto temp = std::make_unique<UnaryNode>(it->getStr(), unaryFuncs, std::move(nodeStack.back()));

                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }

            else if (it->isBinaryOp()) {
                if (nodeStack.size() < 2) {
                    throw std::invalid_argument("Expected argument(s) to binary operator '" + it->getStr() + "'");
                }

                auto temp = std::make_unique<BinaryNode>(it->getStr(), binaryFuncs, std::move(nodeStack.rbegin()[1]), std::move(nodeStack.rbegin()[0]));

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
        nodeStack.pop_back();

        if (root == nullptr) {
            throw std::invalid_argument("Malformed expression, could not generate parse tree");
        }

        self = std::move(root->asString());

    }

    void invalidate() {
        isValid = false;
        root.reset(nullptr);
    }

    bool isValid;

    std::unique_ptr<AstNode> root;

    std::string self;

    std::unordered_map<std::string_view, std::function<T(T,T)>> binaryFuncs;
    std::unordered_map<std::string_view, std::function<T(T)>> unaryFuncs;
    std::unordered_map<std::string, T> variables;

    class AstNode {
    public:
        AstNode() = default;
        AstNode(AstNode&&) = default;
        AstNode(const AstNode&) = default;
        virtual ~AstNode() = default;

        [[nodiscard]] virtual T evaluate() const = 0;
        [[nodiscard]] virtual T evalThreadSafe(const std::unordered_map<std::string, T>& map) const = 0;
        [[nodiscard]] virtual std::unique_ptr<AstNode> clone() const = 0;
        [[nodiscard]] virtual bool validateNode() const = 0;
        [[nodiscard]] virtual std::string asString() const = 0;
        [[nodiscard]] virtual std::unique_ptr<AstNode> derivative(const std::string& wrt, const Expression<T>& parent)  = 0;
    };

    class ValueNode : public AstNode {

    public:
        explicit ValueNode(const Token& token)
                : AstNode(), value(token.convert_to<T>())
        {}

        explicit ValueNode(T value)
                : AstNode(), value(value)
        {}

        ValueNode(ValueNode&& old) noexcept
                : AstNode(), value(std::move(old.value))
        {}

        ValueNode(const ValueNode& old)
                : value(old.value)
        {}

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::move(std::make_unique<ValueNode>(this->value));
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

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, const Expression<T>& parent) {
            return std::make_unique<ValueNode>(0);
        }
    private:
        T value;
    };
    
    class VariableNode : public AstNode {
    public:
        explicit VariableNode(std::string name_, std::unordered_map<std::string, T>& varMap)
                : AstNode(), name(std::move(name_)), variables(varMap)
        {}

        VariableNode(VariableNode&& old) noexcept
                : AstNode(), name(std::move(old.name)), variables(std::move(old.variables))
        {}

        VariableNode(const VariableNode& old)
                : name(old.name), variables(old.variables)
        {}

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::move(std::make_unique<VariableNode>(name, variables));
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

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, const Expression<T>& parent) {
            return std::make_unique<ValueNode>(name == wrt ? 1 : 0);
        }

    private:
        std::string name;
        std::unordered_map<std::string, T>& variables;
    };

    
    class UnaryNode : public AstNode {

    public:
        UnaryNode(const std::string& name, const std::unordered_map<std::string_view, std::function<T(T)>>& functions, std::unique_ptr<AstNode>&& child_)
                : self(name)
        {
            if (!functions.contains(name))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = functions.at(self);

            child = std::move(child_);
        }

        [[deprecated("Does not initialize 'name' field")]]
        UnaryNode(std::function<T(T)> func, std::unique_ptr<AstNode>&& child_)
                : AstNode(), eval(std::move(func)), child(std::move(child_))
        {}

        UnaryNode(UnaryNode&& old) noexcept
                : eval(std::move(old.eval)), child(std::move(old.child))
        {}

        UnaryNode(const UnaryNode& old) {
            *this = old.clone();
        }

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::move(std::make_unique<UnaryNode>(std::function<T(T)>(eval), child->clone()));
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

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, const Expression<T>& parent) {
            return std::make_unique<ValueNode>(0);
        }

    private:
        std::string_view self;
        std::function<T(T)> eval;
        std::unique_ptr<AstNode> child;
    };

    
    class BinaryNode : public AstNode {
    public:
        BinaryNode(const std::string& name, const std::unordered_map<std::string_view, std::function<T(T, T)>>& functions, std::unique_ptr<AstNode>&& leftChild_, std::unique_ptr<AstNode>&& rightChild_)
                : self(name)
        {
            if (!functions.contains(self))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = functions.at(self);

            leftChild = std::move(leftChild_);
            rightChild = std::move(rightChild_);
        }

        [[deprecated("Does not initialize 'name' field")]]
        BinaryNode(std::function<T(T,T)> func, std::unique_ptr<AstNode>&& left, std::unique_ptr<AstNode>&& right)
                : AstNode(), eval(func), leftChild(std::move(left)), rightChild(std::move(right))
        {}

        BinaryNode(BinaryNode&& old) noexcept
                : eval(old.func), leftChild(old.leftChild), rightChild(old.rightChild)
        {}

        BinaryNode(const BinaryNode& old) {
            *this = old.clone();
        }

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::move(std::make_unique<BinaryNode>(std::function<T(T,T)>{eval}, std::move(leftChild->clone()), std::move(rightChild->clone())));
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

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, const Expression<T>& parent) {
            return std::make_unique<ValueNode>(0);
        }
    private:
        std::string_view self;
        std::function<T(const T&, const T&)> eval;
        std::unique_ptr<AstNode> leftChild, rightChild;
    };

};

#endif //AST_EXPRESSION_H
