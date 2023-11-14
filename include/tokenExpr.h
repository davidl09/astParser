//
// Created by davidl09 on 10/26/23.
//

#ifndef AST_TOKENEXPR_H
#define AST_TOKENEXPR_H

#include "tokenizer.h"

class TokenExpression {
public:
    explicit TokenExpression(std::vector<Token> tokens, std::unordered_map<std::string, int> order = {
            {"+", 2},
            {"-", 2},
            {"/", 3},
            {"*", 4},
            {"^", 5}
    }
    )
    : expression(std::move(tokens)),
    operatorStack(),
    inputStack(),
    evalOrder(std::move(order))
    {}

    std::vector<Token> getVariables() {
        std::vector<Token> variables;
        for(const auto& token : expression) {
            if (token.isVariableValue()) {
                variables.push_back(token);
            }
        }
        return variables;
    }

    auto& addImplMultiplication() {
//adds explicit multiplication operators where they are usually implied by convention
        for (auto it = expression.begin(); it < expression.end() - 1; ++it) {
            if((it->isLiteralValue() || it->isRightBracket()) && (it[1].isUnaryOp() || it[1].isValue() || it[1].isLeftBracket())) {
                auto distance = std::distance(expression.begin(), it);
                expression.insert(it + 1, Token{"*", Token::BinaryFuncType});
                it = expression.begin() + distance + 1;
            }
        }
        return *this;
    }

    auto& setUnaryMinFlags() {
        for (auto it = expression.begin(); it < expression.end() - 1; ++it) {
            if (it->isBinaryMinus()) {
                if(
                        it == expression.begin() // '-' sign at beginning is always unary
                        || (it - 1)->isLeftBracket() //'-' sign after left bracket is always unary
                        || (it - 1)->isBinaryOp() // '-' sign after another operator is always unary
                )
                    it->setAsUnaryMinus();
            }
        }
        return *this;
    }

    std::vector<Token> getPostfixExpression() {
        std::vector<Token> output;

        //copy expression to input stack to reverse it
        for (auto it = expression.rbegin(); it < expression.rend(); ++it) {
            //top of inputStack is the beginning of the expression
            inputStack.push(*it);
        }

        while (!inputStack.empty()) {
            if (inputStack.top().isBracket()) {
                handleBracket(output);
            }

            else if (inputStack.top().isValue()) {
                handleValue(output);
            }

            else if (inputStack.top().isUnaryOp()) {
                handleUnaryOp();
            }

            else if (inputStack.top().isBinaryOp()) {
                handleOperator(output);
            }

            else throw std::invalid_argument("Unhandled token");
        }

        while (!operatorStack.empty()) {
            output.emplace_back(operatorStack.top());
            operatorStack.pop();
        }

        return output;
    }

    [[nodiscard]] const auto& getExpression() const {
        return expression;
    }
private:

    auto precedence(const Token& t) const {
        if(t.isBinaryOp())
            return evalOrder.at(t.getStr());
        if(t.isUnaryMinus()) {
            return std::max_element(evalOrder.begin(), evalOrder.end())->second + 1;
        }
        return 1;
    }

    void handleValue(std::vector<Token>& output) {
        output.emplace_back(inputStack.top());
        inputStack.pop();
    }

    void handleBracket(std::vector<Token>& output) {
        if (inputStack.top().isLeftBracket()) {
            operatorStack.push(inputStack.top());
            inputStack.pop();
        } else if (inputStack.top().isRightBracket()) {
            while (!operatorStack.empty() && !operatorStack.top().isLeftBracket()) {
                output.emplace_back(operatorStack.top());
                operatorStack.pop();
            }

            if (!operatorStack.top().isLeftBracket()) throw std::invalid_argument("Expected '('");
            operatorStack.pop();
            if (operatorStack.top().isUnaryOp()) {
                output.emplace_back(operatorStack.top());
                operatorStack.pop();
            }
            inputStack.pop();
            //error if the popped value is not '('
        }
    }

    void handleOperator(std::vector<Token>& output) {
        while (
                !(operatorStack.empty() || operatorStack.top().isLeftBracket())
                && (
                        (precedence(operatorStack.top()) > precedence(inputStack.top())
                        ||
                        (precedence(operatorStack.top()) >= precedence(inputStack.top()) && !inputStack.top().isRightAssociative()))
                    )
                )
        {
            output.emplace_back(operatorStack.top());
            operatorStack.pop();
        }
        operatorStack.push(inputStack.top());
        inputStack.pop();
    }

    void handleUnaryOp() {
        if (inputStack.top().isUnaryOp()) {
            operatorStack.push(std::move(inputStack.top()));
            inputStack.pop();
        }
#ifdef DEBUG
        else throw std::invalid_argument{"Called unary func handler on invalid token type"};
#endif
    }


    std::vector<Token> expression;

    std::stack<Token, std::vector<Token>> operatorStack;
    std::stack<Token, std::vector<Token>> inputStack;

    std::unordered_map<std::string, int> evalOrder;
};


#endif //AST_TOKENEXPR_H
