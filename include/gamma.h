#ifndef GAMMA_H
#define GAMMA_H

#include "token.h"


//source: https://www.rskey.org/CMS/the-library/?view=article&id=11

template<class T>
concept CplxFloat = is_complex_floating_point<T>::value;

template<CplxFloat T>
constexpr inline T gamma_complex(T z) {
    constexpr T p_i[] = {
        1.000000000190015, 
        76.18009172947146, 
        -86.50532032941677, 
        24.01409824083091, 
        -1.231739572450155, 
        1.208650973866179e-3, 
        -5.395239384953e-6
        };

    T p_i_sum = p_i[0];

    for (int i = 1; i <= 6; i++) {
        p_i_sum += p_i[i] / (z + (T)i);
    }

    T result = (p_i_sum * std::numbers::sqrt2 * std::sqrt(std::numbers::pi) / z) * std::pow(z + 5.5, z + 0.5) * std::exp(-(z+5.5));

    return result;
}

#endif