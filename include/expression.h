//
// Created by davidl09 on 10/23/23.
//

#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include <stdexcept>
#include <iostream>

#include "astnode.h"

template<FloatingPoint T>
class Expression {
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

/*    auto asUnaryFunc() const {
        if (!isValid) throw std::runtime_error("Cannot create unary function from invalid expression");
        if (getVariables().size() != 1) throw std::runtime_error("Cannot create unary function from expression containing" + std::to_string(getVariables().size()) + "variables");
        return [*this](T value){
            Expression<T> retFunc(*this);
            retFunc.variables.begin()->second = value;
            return retFunc.evaluate();
        };
    }*/

    const std::string& getExpression() {
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
        std::vector<std::unique_ptr<AstNode<T>>> nodeStack;


        for (auto it = postfixExpression.begin(); it < postfixExpression.end(); ++it) {
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
                if (nodeStack.empty()) {
                    throw std::invalid_argument("Expected argument to unary operator '" + it->getStr() + "'");
                }

                if (!unaryFuncs.contains(it->getStr())) {
                    throw std::invalid_argument("Unknown function encountered: " + it->getStr());
                }
                auto temp = std::make_unique<UnaryNode<T>>(unaryFuncs.at(it->getStr()), std::move(nodeStack.back()));

                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }

            else if (it->isBinaryOp()) {
                if (nodeStack.size() < 2) {
                    throw std::invalid_argument("Expected argument(s) to binary operator '" + it->getStr() + "'");
                }

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
        nodeStack.pop_back();

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

    std::string self;

    std::unordered_map<std::string_view, std::function<T(T,T)>> binaryFuncs;
    std::unordered_map<std::string_view, std::function<T(T)>> unaryFuncs;
    std::unordered_map<std::string, T> variables;
};

#endif //AST_EXPRESSION_H
