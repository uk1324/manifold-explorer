#pragma once

#include <cmath>
#include <Assertions.hpp>

/*
1. if a = 0 then integerGcd(a, b) = b because
r = a % b = 0 because b != 0
a = b
b = r = 0
because b = 0 the loop terminates and the value return is a which is equal to b.

2. if b = 0 then integerGcd(a, b) = a because
then the loop doesn't execute and a is returned

integerGcd(a, b) = 0 <=> a = 0 && b = 0
because 2. implies '<='
and '=>' is true because
if a !=0 and b != 0
then the only way for a to be set to zero is by the line a = b,
but the only way for b to be set to zero is by line b = r,
but that can't happen, because right after that happens the loop terminates.

*/
template<typename T>
T integerGcd(T a, T b) {
	while (b != 0) {
		const auto r = a % b;
		a = b;
		b = r;
	}
	return std::abs(a);
}

template<typename T>
T rationalInCanonicalFormLessThan(T aNumerator, T aDenominator, T bNumerator, T bDenominator) {
	return aNumerator * bDenominator < bNumerator * aDenominator;
}

template<typename T>
T integerToNonNegativePower(T base, T power) {
	ASSERT(power >= 0);
	if (power == 0) {
		return 1;
	}

	if (power % 2 == 0) {
		const auto v = integerToNonNegativePower(base, power / 2);
		return v * v;
	} else {
		return integerToNonNegativePower(base, power - 1);
	}
}

template<typename T>
void rationalNumberInCanonicalFormToIntegerPower(T& numerator, T& denominator, T power) {
	if (power < 0) {
		std::swap(numerator, denominator);
		power = -power;
		if (denominator < 0) {
			// Put the negative sign in the numerator.
			numerator = -numerator;
			denominator = -denominator;
		}
	}
	if (denominator == 0) {
		ASSERT_NOT_REACHED();
		return;
	}
	// The gcd(numerator^power, denominator^power) = gcd(numerator, denominator) = 1, because exponentation doesn't add any new factors that could be cancelled.
	numerator = integerToNonNegativePower(numerator, power);
	denominator = integerToNonNegativePower(denominator, power);
}