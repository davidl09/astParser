//
// Created by davidl09 on 11/15/23.
//

#ifndef AST_CONTEXT_H
#define AST_CONTEXT_H

#include "gamma.h"
#include "mandelbrot.h"

template<CplxOrRealFloat T>
class Context {
public:
    Context() : binaryFuncs({
                        {"+", [](const T &lhs, const T &rhs) -> T { return lhs + rhs; }},
                        {"-", [](const T &lhs, const T &rhs) -> T { return lhs - rhs; }},
                        {"*", [](const T &lhs, const T &rhs) -> T { return lhs * rhs; }},
                        {"/", [](const T &lhs, const T &rhs) -> T { return lhs / rhs; }},
                        {"^", [](const T &lhs, const T &rhs) -> T { return std::pow(lhs, rhs); }},
                }),
                unaryFuncs({
                        {"sqrt", [](const T &arg) -> T { return static_cast<T>(std::sqrt(arg)); }},
                        {"cbrt", [](const T &arg) -> T { return static_cast<T>(std::cbrt(arg)); }},
                        {"exp", [](const T &arg) -> T { return static_cast<T>(std::exp(arg)); }},
                        {"sin", [](const T &arg) -> T { return static_cast<T>(std::sin(arg)); }},
                        {"cos", [](const T &arg) -> T { return static_cast<T>(std::cos(arg)); }},
                        {"tan", [](const T &arg) -> T { return static_cast<T>(std::tan(arg)); }},
                        {"cosh", [](const T &arg) -> T { return static_cast<T>(std::cosh(arg)); }},
                        {"sinh", [](const T &arg) -> T { return static_cast<T>(std::sinh(arg)); }},
                        {"tanh", [](const T &arg) -> T { return static_cast<T>(std::tanh(arg)); }},
                        {"csec", [](const T &arg) -> T { return static_cast<T>(1) / std::sin(arg); }},
                        {"sec", [](const T &arg) -> T { return static_cast<T>(1) / std::cos(arg); }},
                        {"cot", [](const T &arg) -> T { return static_cast<T>(1) / std::tan(arg); }},
                        {"asin", [](const T &arg) -> T { return static_cast<T>(std::asin(arg)); }},
                        {"acos", [](const T &arg) -> T { return static_cast<T>(std::acos(arg)); }},
                        {"atan", [](const T &arg) -> T { return static_cast<T>(std::atan(arg)); }},
                        {"ln", [](const T &arg) -> T { return static_cast<T>(std::log(arg)); }},
                        {"log", [](const T &arg) -> T { return static_cast<T>(std::log10(arg)); }},
                        {"abs", [](const T &arg) -> T { return static_cast<T>(std::abs(arg)); }},
                        {"-", [](const T &arg) -> T { return -arg; }},
                }),
                variables({

                }),
                simpleDerivatives({
                        {"sin", {"sin(x)", "xp*cos(x)"}},
                        {"cos", {"cos(x)", "-xp*sin(x)"}},
                        {"exp", {"exp(x)", "xp*exp(x)"}},
                        {"tan", {"tan(x)", "xp*sec(x)^2"}},
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
                }),
                simplifyRules{
                        //x, n, m, can be replaced with any subtree when substituting
                        {"+", {
                                      {"x + x", "2*x"},
                                      {"n*x+m*x", "(m+n)*x"},

                              }},
                        {"-", {
                                      {"x - x", "0"},
                                      {"0-x", "-x"},
                              }},
                        {"*", {
                                      {"x * 0", "0"},
                                      {"x * 1", "x"},
                                      {"x^n*x^m", "x^(m+n)"},
                                      {"x*x^n", "x^(n+1)"},
                                      {"x ^ 1", "x"},
                              }},
                        {"/", {
                                      {"x^n/x^m", "x^(n-m)"},
                                      {"(x^n)/x", "x^(n-1)"},
                                      {"1/(x^-n)", "x^n"},
                                      {"n/x", "n*x^(-1)"},
                              }},
                        {"^", {
                                      {"x^1", "x"},
                                      {"x^0", "1"},
                                      {"0^x", "0"},
                              }},
                } {
        if constexpr (is_complex_floating_point<T>::value) {
            unaryFuncs["mandelbrot"] = mandelbrot<T>;
            unaryFuncs["arg"] = [](const T &arg) -> T { return std::arg(arg); };
            unaryFuncs["real"] = [](const T &arg) -> T { return std::real(arg); };
            unaryFuncs["imag"] = [](const T &arg) -> T { return std::imag(arg); };
            unaryFuncs["gamma"] = gamma_complex<T>;
        }
    }

    auto &getVars() {
        return variables;
    }

    auto &getUnaryFuncs() {
        return unaryFuncs;
    }

    auto &getBinaryFuncs() {
        return binaryFuncs;
    }

    auto &getDerivatives() {
        return simpleDerivatives;
    }

    const auto &getSimplifyRules() {
        return simplifyRules;
    }


private:
    std::unordered_map<std::string_view, std::function<T(T, T)>> binaryFuncs;
    std::unordered_map<std::string_view, std::function<T(T)>> unaryFuncs;
    std::unordered_map<std::string, T> variables;
    std::unordered_map<std::string_view, std::pair<std::string_view, std::string_view>> simpleDerivatives;
    std::unordered_map<std::string_view, std::unordered_map<std::string_view, std::string_view>> simplifyRules;
};


#endif//AST_CONTEXT_H
