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
                    {"cbrt", [](const T &arg) -> T { return static_cast<T>(std::cbrt(arg)); }},
                    {"exp",  [](const T &arg) -> T { return static_cast<T>(std::exp(arg)); }},
                    {"sin",  [](const T &arg) -> T { return static_cast<T>(std::sin(arg)); }},
                    {"cos",  [](const T &arg) -> T { return static_cast<T>(std::cos(arg)); }},
                    {"tan",  [](const T &arg) -> T { return static_cast<T>(std::tan(arg)); }},
                    {"cosh",  [](const T &arg) -> T { return static_cast<T>(std::cosh(arg)); }},
                    {"sinh",  [](const T &arg) -> T { return static_cast<T>(std::sinh(arg)); }},
                    {"tanh",  [](const T &arg) -> T { return static_cast<T>(std::tanh(arg)); }},
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
        : isValid(old.isValid), root(old.root ? old.root->clone() : nullptr), self(old.self), binaryFuncs(old.binaryFuncs), unaryFuncs(old.unaryFuncs), variables(old.variables)
    {}

    Expression(Expression&& old) noexcept
    : isValid(old.isValid), root(old.root ? std::move(old.root) : nullptr), self(old.self), binaryFuncs(std::move(old.binaryFuncs)), unaryFuncs(std::move(old.unaryFuncs)), variables(std::move(old.variables))
    {}

    Expression() {
        *this = Expression<T>("0");
        isValid = true;
    }

    Expression derivative(const std::string& wrt) {
        Expression result;
        result.root = this->root->derivative(wrt, result);

        result.optimize();

        result.updateStrRepr();
        return result;
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

        Expression<T> copy(*this);

        return [copy](const std::unordered_map<std::string, T>& vars = {{}}){
            return copy.evaluate(vars);
        };
    }

    const std::string& string() const {
        return self;
    }

    void optimize() {
        root = root->optimize();
        updateStrRepr();
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
        optimize();
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
            {"sqrt", {"sqrt(x)", "xp/(2*sqrt(x))"}},
            {"cbrt", {"cbrt(x)", "xp/(3*cbrt(x)*cbrt(x))"}},
            {"sinh", {"sinh(x)", "xp*cosh(x)"}},
            {"cosh", {"cosh(x)", "xp*sinh(x)"}},
            {"tanh", {"tanh(x)", "xp*(1 - tanh(x)*tanh(x))"}},
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
        [[nodiscard]] virtual std::string selfId() const = 0;
        [[nodiscard]] virtual std::string asString() const = 0;
        [[nodiscard]] virtual std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) = 0;
        [[nodiscard]] virtual std::unique_ptr<AstNode> optimize() = 0;
        [[nodiscard]] virtual bool noVariableNodes() = 0;

        virtual bool swapVarWithSubTree(const std::unique_ptr<AstNode>& subtree, const std::string& toBeReplaced) = 0;


        friend bool operator==(const std::unique_ptr<AstNode>& lhs, const std::unique_ptr<AstNode>& rhs) {
            return lhs->asString() == rhs->asString();
        }
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

        [[nodiscard]] std::string selfId() const {
            return asString();
        }

        [[nodiscard]] std::string asString() const {
            std::stringstream s;
            s << std::real(value);
            return s.str();
        }

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            return std::make_unique<ValueNode>(0);
        }

        [[nodiscard]] std::unique_ptr<AstNode> optimize() {
            return std::make_unique<ValueNode>(std::move(*this));
        };

        [[nodiscard]] bool noVariableNodes() {
            return true;
        }

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
                : AstNode(), name(std::move(old.name)), variables(old.variables)
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

        [[nodiscard]] std::string selfId() const {
            return name;
        }

        [[nodiscard]] std::string asString() const {
            return selfId();
        }

        [[nodiscard]] std::unique_ptr<AstNode> derivative(const std::string& wrt, Expression& ctx) {
            return std::make_unique<ValueNode>(name == wrt ? 1 : 0);
        }

        [[nodiscard]] std::unique_ptr<AstNode> optimize() {
            return std::make_unique<VariableNode>(std::move(*this));
        }

        [[nodiscard]] bool noVariableNodes() {
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
        UnaryNode(const std::string& name, Expression& parent, std::unique_ptr<AstNode>&& child_)
                : self(name), context(parent)
        {
            if (!context.unaryFuncs.contains(name))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = context.unaryFuncs.at(self);

            child = std::move(child_);
        }

        UnaryNode(UnaryNode&& old) noexcept
                : self(old.self), eval(old.eval), child(std::move(old.child)), context(old.context)
        {}

        UnaryNode(const UnaryNode& old) noexcept
                : self(old.self), eval(old.eval), child(old.child->clone()), context(old.context)
        {}

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

        [[nodiscard]] std::string selfId() const {
            return self;
        }

        [[nodiscard]] std::string asString() const {
            return self + "(" + child->asString() + ")";
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
        
        [[nodiscard]] std::unique_ptr<AstNode> optimize() {
            if (child->noVariableNodes()) {
                return std::make_unique<ValueNode>(evaluate());
            }
            child = child->optimize();
            return std::make_unique<UnaryNode>(std::move(*this));
        }

        [[nodiscard]] bool noVariableNodes() {
            return child->noVariableNodes();
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
        Expression& context;
    };

    
    class BinaryNode : public AstNode {
    public:
        BinaryNode(std::string name, Expression& parent, std::unique_ptr<AstNode>&& leftChild_, std::unique_ptr<AstNode>&& rightChild_)
                : self(std::move(name)), context(parent)
        {
            if (!context.binaryFuncs.contains(self))
                throw std::invalid_argument("Tried initializing unary function node with invalid name");
            eval = context.binaryFuncs.at(self);

            leftChild = std::move(leftChild_);
            rightChild = std::move(rightChild_);
        }

        BinaryNode(BinaryNode&& old) noexcept
                : self(old.self), eval(old.eval), leftChild(std::move(old.leftChild)), rightChild(std::move(old.rightChild)), context(old.context)
        {}

        BinaryNode(const BinaryNode& old)
                : self(old.self), eval(old.eval), leftChild(old.leftChild->clone()), rightChild(old.rightChild->clone()), context(old.context)
        {}

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

        [[nodiscard]] std::string selfId() const {
            return self;
        }

        [[nodiscard]] std::string asString() const {
            return "(" + leftChild->asString() + ")" + self + "(" + rightChild->asString() + ")";
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
            else if (self == "^") {
                                //if h(x) = f(x)^g(x), then h'(x) =    h(x) * (g'(x) * ln(f(x)) + g(x) * f'(x) / f(x))
                auto f = leftChild->clone();//                                                           ^^^^^C^^^^^
                auto fp = leftChild->derivative(wrt, ctx);//              ^^^^^^A^^^^^^^   ^^^^^^^^^^B^^^^^^^^^^^
                auto g = rightChild->clone();//                              ^^^^^^^^^^^^^^^^^D^^^^^^^^^^^^^^^^^^^^^
                auto gp = rightChild->derivative(wrt, ctx);
                auto h = std::make_unique<BinaryNode>("^", ctx, f->clone(), g->clone());
                auto C = std::make_unique<BinaryNode>("/", ctx, fp->clone(), f->clone());
                auto B = std::make_unique<BinaryNode>("*", ctx, g->clone(), std::move(C)); //C is now invalid in this scope
                auto A = std::make_unique<BinaryNode>("*", ctx, gp->clone(), std::make_unique<UnaryNode>("ln", ctx, f->clone()));
                auto D = std::make_unique<BinaryNode>("+", ctx, std::move(A), std::move(B));
                return std::make_unique<BinaryNode>("*", ctx, std::move(h), std::move(D));
            }
            else throw std::invalid_argument("Derivative for operator " + std::string{self} + " is not defined");
        }

        [[nodiscard]] std::unique_ptr<AstNode> optimize() {
            auto leftNoVar = leftChild->noVariableNodes(), rightNoVar = rightChild->noVariableNodes();

            if (leftChild == rightChild) {
                if (self == "+") {
                    return std::make_unique<BinaryNode>("*", context, std::make_unique<ValueNode>(2), std::move(leftChild));
                }
                if (self == "-") {
                    return std::make_unique<ValueNode>(0);
                }
                if (self == "*") {
                    return std::make_unique<BinaryNode>("^", context, std::move(leftChild), std::make_unique<ValueNode>(2));
                }
                if (self == "/") {
                    return std::make_unique<ValueNode>(1);
                }
            }

            if (!(leftNoVar || rightNoVar)) {
                leftChild = leftChild->optimize();
                rightChild = rightChild->optimize();
            }

            else if(leftNoVar && rightNoVar) {
                return std::make_unique<ValueNode>(evaluate());
            }

            else { //at this point either left or right no var is true;
                auto& constNode = (leftNoVar ? leftChild : rightChild);
                auto& varNode = (leftNoVar ? rightChild : leftChild);

                constNode = std::make_unique<ValueNode>(constNode->evaluate());

                if (self == "+") {
                    if (constNode->evaluate() == static_cast<T>(0)) {
                        return varNode->clone();
                    }
                }

                if (self == "-") {
                    if (leftNoVar && leftChild->evaluate() == static_cast<T>(0)) {
                        return std::move(rightChild);
                    }
                    if (rightNoVar && rightChild->evaluate() == static_cast<T>(0)) {
                        return std::make_unique<UnaryNode>("-", context, std::move(leftChild));
                    }
                }

                if (self == "*") {
                    if (constNode->evaluate() == static_cast<T>(0)) {
                        return std::make_unique<ValueNode>(0);
                    }
                    if (constNode->evaluate() == static_cast<T>(1)) {
                        return varNode->clone();
                    }
                }

                if (self == "/") {
                    if (leftNoVar && leftChild->evaluate() == static_cast<T>(0)) {  // 0/x == 0
                        return std::make_unique<ValueNode>(0);
                    }
                    if (rightNoVar && rightChild->evaluate() == static_cast<T>(1)) {  // x/1 == x
                        return std::move(leftChild);
                    }
                    if (rightNoVar && rightChild->evaluate() == 0) {
                        return std::make_unique<ValueNode>(static_cast<T>(1)/static_cast<T>(0));
                    }
                }

                if (self == "^") {
                    if (leftNoVar) {
                        auto val = leftChild->evaluate();
                        if(val == static_cast<T>(0))
                            return std::make_unique<ValueNode>(0);
                        else if (val == static_cast<T>(1))
                            return std::move(rightChild);

                    }
                    if (rightNoVar) {
                        auto val = rightChild->evaluate();
                        if (val == static_cast<T>(0))
                            return std::make_unique<ValueNode>(0);
                        if (val == static_cast<T>(1))
                            return std::make_unique<ValueNode>(1); // 1^x == 1 for all x
                    }
                }
            }
            return std::make_unique<BinaryNode>(std::move(*this));
        }

        [[nodiscard]] bool noVariableNodes() {
            return leftChild->noVariableNodes() && rightChild->noVariableNodes();
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
        Expression& context;
    };

};

#endif //AST_EXPRESSION_H
