//
// Created by davidl09 on 10/23/23.
//

#ifndef AST_EXPRESSION_H
#define AST_EXPRESSION_H

#include <stdexcept>
#include <iostream>
#include <utility>

#include "tokenExpr.h"

#include "gamma.h"
#include "mandelbrot.h"

template<CplxOrRealFloat T>
class Expression {
private:
    class AstNode;
    class ValueNode;
    class VariableNode;
    class UnaryNode;
    class BinaryNode;
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
    {
        *this = Expression(old.self);
    }

    Expression(Expression&& old) noexcept
    : isValid(old.isValid), root(old.root ? std::move(old.root) : nullptr), binaryFuncs(std::move(old.binaryFuncs)), unaryFuncs(std::move(old.unaryFuncs)), variables(std::move(old.variables)), self(old.self)
    {}

    Expression() {
        *this = Expression<T>("0");
        isValid = true;
    }

    Expression derivative(const std::string& wrt) {
        Expression result;
        result.root = this->root->derivative(wrt, result);
        //result.optimize();
        result.updateStrRepr();
        return result;
    }

    void optimize() {
        if (root->optimize()) {
            root = std::make_unique<ValueNode>(root->evaluate());
            self = root->asString();
        }
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

    const auto &getBinaryFunc(const std::string& name) const {
        return binaryFuncs.at(name);
    }

    const auto &getUnaryFunc(const std::string& name) const {
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

        Expression<T> copy = *this;

        return [copy](const std::unordered_map<std::string, T>& vars = {{}}){
            return copy.evaluate(vars);
        };
    }

    const std::string& string() const {
        return self;
    }

private:

    Expression(std::unique_ptr<AstNode> root_) {
        Expression e;
        e.root = std::move(root_);
        e.self = e.root->asString();
    }

    void updateStrRepr() {
        if (!(root != nullptr && root->validateNode())) {
            invalidate();
            return;
        }
        self = root->asString();

        //update variable table
        auto tokenExpression = tokenizeExpression(self);
        auto tempVars = tokenExpression.getVariables();
        variables.clear();

        std::for_each(tempVars.begin(), tempVars.end(), [&](const Token& t){variables[t.getStr()];});
    }

    static TokenExpression tokenizeExpression(const std::string& expression) {
        if (expression.empty()) {
            throw std::invalid_argument("Cannot initialize empty expression");
        }

        Tokenizer tokenizer(expression);
        if (!tokenizer.isValidCharExpr()) throw std::invalid_argument("Invalid expression");
        return TokenExpression{tokenizer.tokenize()};
    }

    static std::unique_ptr<AstNode> makeExprTree(TokenExpression tokenExpression, Expression& context) {
        auto postfixExpression = tokenExpression.setUnaryMinFlags().addImplMultiplication().getPostfixExpression();
        std::vector<std::unique_ptr<AstNode>> nodeStack;


        for (auto it = postfixExpression.begin(); it < postfixExpression.end(); ++it) {
            if (it->isVariableValue()) {
                nodeStack.emplace_back(
                        std::make_unique<VariableNode>(it->getStr(), context.variables)
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

                if (!context.unaryFuncs.contains(it->getStr())) {
                    throw std::invalid_argument("Unknown function encountered: " + it->getStr());
                }
                auto temp = std::make_unique<UnaryNode>(it->getStr(), context, std::move(nodeStack.back()));

                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }

            else if (it->isBinaryOp()) {
                if (nodeStack.size() < 2) {
                    throw std::invalid_argument("Expected argument(s) to binary operator '" + it->getStr() + "'");
                }

                auto temp = std::make_unique<BinaryNode>(it->getStr(), context, std::move(nodeStack.rbegin()[1]), std::move(nodeStack.rbegin()[0]));

                nodeStack.pop_back();
                nodeStack.pop_back();
                nodeStack.emplace_back(std::move(temp));
            }
        }

        if (nodeStack.size() != 1) {
            std::cout << nodeStack.size();
            throw std::invalid_argument("Unbalanced equation");
        }

        return std::move(nodeStack.back());
    }

    void init(const std::string& expression) {

        auto tokenExpression = tokenizeExpression(expression);

        auto tempVars = tokenExpression.getVariables();
        variables.clear();

        std::for_each(tempVars.begin(), tempVars.end(), [&](const Token& t){variables[t.getStr()];});


        root = makeExprTree(tokenExpression, *this);

        self = root->asString();

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

    const std::unordered_map<std::string_view, std::pair<std::string_view, std::string_view>> simpleDerivatives {
            {"sin", {"sin(x)", "xp*cos(x)"}},
            {"cos", {"cos(x)", "-xp*sin(x)"}},
            {"exp", {"exp(x)", "xp*exp(x)"}},
            {"tan", {"tan(x)", "xp*sec(x)^2"}},
            {"log", {"log(x)", "xp/x"}},
            {"csec", {"csec(x)", "-xp*csc(x)*cot(x)"}},
            {"sec", {"sec(x)", "xp*sec(x)*tan(x)"}},
            {"cot", {"cot(x)", "-xp*(1 + cot(x)*cot(x))"}},
            {"asin", {"asin(x)", "xp/sqrt(1 - x*x)"}},
            {"acos", {"acos(x)", "-xp/sqrt(1 - x*x)"}},
            {"atan", {"atan(x)", "xp/(1 + x^2)"}},
            {"ln", {"ln(x)", "xp/x"}},
            {"log", {"log(x)", "xp/(x * ln(10))"}},
    };

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
        [[nodiscard]] virtual std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) = 0;
        virtual bool optimize() = 0;
        virtual bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) = 0;
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
            return std::to_string(std::real(this->value));
        }

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            return std::make_unique<ValueNode>(0);
        }
        
        bool optimize() {
            return true;
        };

        bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) {
            return false;
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

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            return std::make_unique<ValueNode>(name == wrt ? 1 : 0);
        }
        
        bool optimize() {
            return false;
        }

        bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) {
            return name == toBeReplaced;
        }

    private:
        std::string name;
        std::unordered_map<std::string, T>& variables;
    };

    
    class UnaryNode : public AstNode {

    public:
        UnaryNode(const std::string& name, const Expression& parent, std::unique_ptr<AstNode>&& child_)
                : self(name), context(parent)
        {
            if (!context.unaryFuncs.contains(name))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = context.unaryFuncs.at(self);

            child = std::move(child_);
        }

        UnaryNode(UnaryNode&& old) noexcept
        {
            self = old.self;
            eval = old.eval;
            child = std::move(old.child);
            context = old.context;
        }

        UnaryNode(const UnaryNode& old) noexcept {
            self = old.self;
            eval = old.eval;
            child = old.child->clone();
            context = old.context;
        }

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::move(std::make_unique<UnaryNode>(self, context, child->clone()));
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
            return std::string{self} + "(" + child->asString() + ")";
        }

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            if (ctx.simpleDerivatives.contains(self)) {
                auto xp = child->derivative(wrt, ctx);
                auto x = child->clone();

                auto dTree = makeExprTree(tokenizeExpression(std::string{ctx.simpleDerivatives.at(self).second}), ctx);

                dTree->swapVarWithSubTree(x, "x");
                dTree->swapVarWithSubTree(xp, "xp");

                return dTree;

            }
            else throw std::invalid_argument("Derivative for " + std::string{self} + " is not implemented");
        }
        
        bool optimize() {
            if (child->optimize()) {
                child = std::make_unique<ValueNode>(child->evaluate());
                return true;
            }
            return false;
        }

        bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) {
            if (child->swapVarWithSubTree(subtree, toBeReplaced)) {
                child = subtree->clone();
            }
            return false;
        }
    private:
        std::string self;
        std::function<T(T)> eval;
        std::unique_ptr<AstNode> child;
        const Expression& context;
    };

    
    class BinaryNode : public AstNode {
    public:
        BinaryNode(std::string name, const Expression& parent, std::unique_ptr<AstNode>&& leftChild_, std::unique_ptr<AstNode>&& rightChild_)
                : self(std::move(name)), context(parent)
        {
            if (!context.binaryFuncs.contains(self))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = context.binaryFuncs.at(self);

            leftChild = std::move(leftChild_);
            rightChild = std::move(rightChild_);
        }

        BinaryNode(BinaryNode&& old) noexcept {
            self = old.self;
            eval = old.eval;
            leftChild = std::move(old.leftChild);
            rightChild = std::move(old.rightChild);
            context = old.context;
        }

        BinaryNode(const BinaryNode& old) {
            self = old.self;
            eval = old.eval;
            leftChild = old.leftChild->clone();
            rightChild = old.rightChild->clone();
            context = old.context;
        }

        [[nodiscard]] std::unique_ptr<AstNode> clone() const final {
            return std::make_unique<BinaryNode>(self, context, leftChild->clone(), rightChild->clone());
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
            return "(" + leftChild->asString() + ")" + std::string{self} + "(" + rightChild->asString() + ")";
        }

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            if (self == "+" || self == "-") {
                return std::make_unique<BinaryNode>(self, ctx, std::move(leftChild->derivative(wrt, ctx)), rightChild->derivative(wrt, ctx));
            }
            else if (self == "*") {
                auto lhs = std::make_unique<BinaryNode>("*", ctx, leftChild->derivative(wrt, ctx), rightChild->clone());
                auto rhs = std::make_unique<BinaryNode>("*", ctx, leftChild->clone(), rightChild->derivative(wrt, ctx));
                return std::make_unique<BinaryNode>("+", ctx, std::move(lhs), std::move(rhs));
            }
            else if (self == "/") {
                auto lhsNumer = std::make_unique<BinaryNode>("*", ctx, leftChild->derivative(wrt, ctx), rightChild->clone());
                auto rhsNumer = std::make_unique<BinaryNode>("*", ctx, leftChild->clone(), rightChild->derivative(wrt, ctx));
                auto numer = std::make_unique<BinaryNode>("-", ctx, std::move(lhsNumer), std::move(rhsNumer));

                auto denom = std::make_unique<BinaryNode>("^", ctx, rightChild->clone(), std::make_unique<ValueNode>(2));

                return std::move(std::make_unique<BinaryNode>("/", ctx, std::move(numer), std::move(denom)));
            }
            else throw std::invalid_argument("Derivative for operator " + std::string{self} + " is not defined");
        }

        bool optimize() {
            auto left = leftChild->optimize(), right = rightChild->optimize();
            if (!(left && right)) {
                if (left) {
                    leftChild = std::make_unique<ValueNode>(leftChild->evaluate());

                }
                else if (right) {
                    rightChild = std::make_unique<ValueNode>(rightChild->optimize());
                }
                if (self == "*") {
                    if (right || left) {
                        if ((right ? rightChild : leftChild)->evaluate() == static_cast<T>(0)) {
                            return true;
                        }
                    }
                }
                return false;
            }
            else return true;
        }

        bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) {
            bool left = leftChild->swapVarWithSubTree(subtree, toBeReplaced),
                 right = rightChild->swapVarWithSubTree(subtree, toBeReplaced);
            if (left) {
                leftChild = subtree->clone();
            }
            if (right) {
                rightChild = subtree->clone();
            }
            return false;
        }
    private:
        std::string self;
        std::function<T(const T&, const T&)> eval;
        std::unique_ptr<AstNode> leftChild, rightChild;
        const Expression& context;
    };

};

#endif //AST_EXPRESSION_H
