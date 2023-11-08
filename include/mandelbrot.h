#ifndef MANDELBROT_H
#define MANDELBROT_H

#include "gamma.h" //include cplxfloat concept

template<CplxFloat T>
constexpr inline T mandelbrot(T c) {
    constexpr int maxIter = 80;
    int n = 0;
    T z = 0;
    T i_c = {0,1};

    while (std::abs(z) <= 2 && n < maxIter) {
        z = z * z + c;
        ++n;
    }

    if (n == maxIter) return 0;

    auto arg = 2 * std::numbers::pi * static_cast<double>(n) / maxIter - std::numbers::pi;

    auto result = n + 1 - std::log(std::log2(std::abs(z)));

    return result * std::exp(i_c * arg);
}

#endif