#include "Simplification.hpp"
#include "../Arithmetic.hpp"
#include <Put.hpp>
#include "Context.hpp"
#include "PrintExpr.hpp"
#include <View.hpp>
#include <Assertions.hpp>
#include <optional>

using namespace Algebra;

View<const AlgebraicExprPtr> rest(View<const AlgebraicExprPtr> view) {
	return View<const AlgebraicExprPtr>(view.data() + 1, view.size() - 1);
};

AlgebraicExprPtr basicSimplifyProduct(const Context& c, const AlgebraicExprList& factors);

std::optional<AlgebraicExprPtr> algebraicExprBase(const Context& c, const AlgebraicExprPtr& exprPtr) {
	switch (exprPtr->type) {
		using enum AlgebraicExprType;
		case INTEGER:
		case RATIONAL:
			return std::nullopt;

		case DERIVATIVE: // A derivative could thechnically be a constant if it is not simplified fully.
		case SYMBOL:
		case FUNCTION:
		case SUM:
		case PRODUCT:
			return algebraicExprClone(c, exprPtr);

		case POWER: {
			const auto e = static_cast<const PowerExpr*>(exprPtr.get());
			// @Performance
			return algebraicExprClone(c, e->base);
		}
	}
	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

std::optional<AlgebraicExprPtr> algebraicExprExponent(const Context& c, const AlgebraicExprPtr& exprPtr) {
	const auto expr = exprPtr.get();
	switch (exprPtr->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return std::nullopt;

	case SYMBOL:
	case FUNCTION:
	case SUM:
	case PRODUCT:
	case DERIVATIVE:
		return std::make_unique<IntegerExpr>(1);
	case POWER: {
		const auto e = static_cast<const PowerExpr*>(exprPtr.get());
		return algebraicExprClone(c, e->exponent);
	}
	}
	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

bool algebraicExprListEquals(const AlgebraicExprList& a, const AlgebraicExprList& b) {
	if (a.size() != b.size()) {
		return false;
	}
	for (i32 i = 0; i < a.size(); i++) {
		if (!algebraicExprEquals(a[i], b[i])) {
			return false;
		}
	}
	return true;
}

bool symbolEquals(const Symbol* a, const Symbol* b) {
	return a == b;
}

bool Algebra::algebraicExprEquals(const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr) {
	const auto aExpr = aExprPtr.get();
	const auto bExpr = bExprPtr.get();

	if (aExpr->type != bExpr->type) {
		return false;
	}

	switch (aExpr->type) {
		using enum AlgebraicExprType;
	case INTEGER: {
		const auto a = static_cast<const IntegerExpr*>(aExpr);
		const auto b = static_cast<const IntegerExpr*>(bExpr);
		return a->value == b->value;
	}
	case RATIONAL: {
		const auto a = static_cast<const RationalExpr*>(aExpr);
		const auto b = static_cast<const RationalExpr*>(bExpr);
		return a->numerator == b->numerator && a->denominator == b->denominator;
	}

	case SYMBOL: {
		const auto a = static_cast<const SymbolExpr*>(aExpr);
		const auto b = static_cast<const SymbolExpr*>(bExpr);
		return symbolEquals(a->symbol, b->symbol);
	}
	case FUNCTION: {
		const auto a = static_cast<const FunctionExpr*>(aExpr);
		const auto b = static_cast<const FunctionExpr*>(bExpr);
		if (a->function->name != b->function->name) {
			return false;
		}
		return algebraicExprListEquals(a->arguments, b->arguments);
	}
	case SUM: {
		const auto a = static_cast<const SumExpr*>(aExpr);
		const auto b = static_cast<const SumExpr*>(bExpr);
		return algebraicExprListEquals(a->summands, b->summands);
	}
	case PRODUCT: {
		const auto a = static_cast<const ProductExpr*>(aExpr);
		const auto b = static_cast<const ProductExpr*>(bExpr);
		return algebraicExprListEquals(a->factors, b->factors);
	}
	case POWER: {
		const auto a = static_cast<const PowerExpr*>(aExpr);
		const auto b = static_cast<const PowerExpr*>(bExpr);
		return algebraicExprEquals(a->base, b->base) &&
			algebraicExprEquals(a->exponent, b->exponent);
	}
	case DERIVATIVE: {
		const auto a = aExpr->asDerivative();
		const auto b = bExpr->asDerivative();
		return algebraicExprEquals(a->expr, b->expr) && symbolEquals(a->symbol, b->symbol);
	}

	}

	CHECK_NOT_REACHED();
	return false;
}

bool algebraicExprViewLessThan(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	for (i32 i = 0; ; i++) {
		const auto aIndex = i64(a.size()) - 1 - i;
		const auto bIndex = i64(b.size()) - 1 - i;
		if (aIndex < 0 || bIndex < 0) {
			break;
		}
		if (!algebraicExprEquals(a[aIndex], b[bIndex])) {
			return algebraicExprLessThan(c, a[aIndex], b[bIndex]);
		}
	}
	return a.size() < b.size();
}

bool algebraicExprListLessThan(const Context& c, const AlgebraicExprList& a, const AlgebraicExprList& b) {
	return algebraicExprViewLessThan(c, constView(a), constView(b));
}

bool algebraicExprPowerLessThan(const Context& c, const AlgebraicExprPtr& aBase, const AlgebraicExprPtr& aExponent,
	const AlgebraicExprPtr& bBase, const AlgebraicExprPtr& bExponent) {
	if (algebraicExprEquals(aBase, bBase)) {
		return algebraicExprLessThan(c, aExponent, bExponent);
	} else {
		return algebraicExprLessThan(c, aBase, bBase);
	}
}

bool symbolLessThan(const Symbol* a, const Symbol* b) {
	if (a->name != b->name) {
		return a->name < b->name;
	}
	return intptr_t(a) < intptr_t(b);
}

// To make things more explicit could have each comparasion to have a separate function and return the function or it's negation.
bool Algebra::algebraicExprLessThan(const Context& c, const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr) {
#define CASE_FUNCTION_LIKE \
	case FUNCTION: \
	case DERIVATIVE:
	const auto aExpr = aExprPtr.get();
	const auto bExpr = bExprPtr.get();

	switch (aExpr->type) {
		using enum AlgebraicExprType;
	case INTEGER: {
		const auto a = static_cast<const IntegerExpr*>(aExpr);
		switch (bExpr->type) {
		case INTEGER: {
			const auto b = static_cast<const IntegerExpr*>(bExpr);
			return a->value < b->value;
		}
				
		case RATIONAL: {
			const auto b = static_cast<const RationalExpr*>(bExpr);
			return rationalInCanonicalFormLessThan(a->value, IntegerType(1), b->numerator, b->denominator);
		}

		case SYMBOL:
		CASE_FUNCTION_LIKE
		case SUM:
		case PRODUCT:
		case POWER:
			return true;
			
		}
		break;
	}
	case RATIONAL: {
		const auto a = static_cast<const RationalExpr*>(aExpr);
		switch (bExpr->type) {
		case INTEGER: {
			const auto b = static_cast<const IntegerExpr*>(bExpr);
			return rationalInCanonicalFormLessThan(a->numerator, a->denominator, b->value, IntegerType(1));;
		}

		case RATIONAL: {
			const auto b = static_cast<const RationalExpr*>(bExpr);
			return rationalInCanonicalFormLessThan(a->numerator, a->denominator, b->numerator, b->denominator);
		}

		case SYMBOL:
		CASE_FUNCTION_LIKE
		case SUM:
		case PRODUCT:
		case POWER:
			return true;
		}
		break;
	}
	case SYMBOL: {
		const auto a = static_cast<const SymbolExpr*>(aExpr);
		switch (bExpr->type) {
		case SYMBOL: {
			const auto b = static_cast<const SymbolExpr*>(bExpr);
			return symbolLessThan(a->symbol, b->symbol);
		}
		case INTEGER: 
		case RATIONAL: 
		CASE_FUNCTION_LIKE
		case SUM:
		case PRODUCT:
		case POWER:
			break;
		}
		break;
	}
#define FUNCTION_LIKE_BREAK \
	case INTEGER: \
	case RATIONAL: \
	case SUM: \
	case PRODUCT: \
	case POWER: \
	break;

	case FUNCTION: {
		const auto a = static_cast<const FunctionExpr*>(aExpr);
		switch (bExpr->type) {
		case SYMBOL: {
			const auto b = static_cast<const SymbolExpr*>(bExpr);
			if (a->function->name == b->symbol->name) {
				// Put variables on the left of functions
				return i32(a->function->type) < i32(b->symbol->type);
			} else {
				return a->function->name < b->symbol->name;
			}
		}
		case FUNCTION: {
			const auto b = static_cast<const FunctionExpr*>(bExpr);
			if (a->function->name != b->function->name) {
				return a->function->name < b->function->name;
			}
			if (a->function->type != b->function->type) {
				return i32(a->function->type) < i32(b->function->type);
			}
			return algebraicExprListLessThan(c, a->arguments, b->arguments);
		}
		case DERIVATIVE: {
			// FUNCTION < DERIVATIVE
			const auto b = static_cast<const DerivativeExpr*>(bExpr);
			const auto derivativeName = "D";
			if (a->function->name != derivativeName) {
				return a->function->name < derivativeName;
			}
			// @Performance: 
			AlgebraicExprList arguments;
			arguments.push_back(algebraicExprClone(c, b->expr));
			arguments.push_back(std::make_unique<SymbolExpr>(b->symbol));
			return algebraicExprListLessThan(c, a->arguments, arguments);
		}
		FUNCTION_LIKE_BREAK
		}
		break;
	}
	case DERIVATIVE: {
		const auto a = aExpr->asDerivative();
		switch (bExpr->type) {
		case DERIVATIVE: {
			const auto b = bExpr->asDerivative();
			if (!symbolEquals(a->symbol, b->symbol)) {
				return symbolLessThan(a->symbol, b->symbol);
			}
			return algebraicExprLessThan(c, a->expr, b->expr);
		}
		case SYMBOL:
		case FUNCTION:
			break;
		FUNCTION_LIKE_BREAK
		}
		break;
	}
	case SUM: {
		const auto a = static_cast<const SumExpr*>(aExpr);
		switch (bExpr->type) {
		case SUM: {
			const auto b = static_cast<const SumExpr*>(bExpr);
			return algebraicExprListLessThan(c, a->summands, b->summands);
		}
		case SYMBOL:
		CASE_FUNCTION_LIKE
			return algebraicExprViewLessThan(c, constView(a->summands), constView(bExprPtr));

		case PRODUCT:
		case POWER:
		case INTEGER:
		case RATIONAL:
			break;
		}
		break;
	}
	case PRODUCT: {
		const auto a = static_cast<const ProductExpr*>(aExpr);
		switch (bExpr->type) {
		case PRODUCT: {
			const auto b = static_cast<const ProductExpr*>(bExpr);
			return algebraicExprListLessThan(c, a->factors, b->factors);
		}
		case SYMBOL:
		CASE_FUNCTION_LIKE
		case SUM:
		case POWER:
			return algebraicExprViewLessThan(c, constView(a->factors), constView(bExprPtr));

		case INTEGER:
		case RATIONAL:
			break;
		}
		break;
	}
	case POWER: {
		const auto a = static_cast<const PowerExpr*>(aExpr);
		switch (bExpr->type) {
		case POWER: {
			const auto b = static_cast<const PowerExpr*>(bExpr);
			return algebraicExprPowerLessThan(c, a->base, a->exponent, b->base, b->exponent);
		}

		case SYMBOL:
		CASE_FUNCTION_LIKE
		case SUM:
			// @Performance: allocating. Could preallocate some constants for things like this.
			return algebraicExprPowerLessThan(c, a->base, a->exponent, bExprPtr, std::make_unique<IntegerExpr>(1));

		case PRODUCT:
		case INTEGER:
		case RATIONAL:
			break;
		}
		break;
	}
	}

	if (algebraicExprEquals(aExprPtr, bExprPtr)) {
		return false;
	}

	return !algebraicExprLessThan(c, bExprPtr, aExprPtr);
}
AlgebraicExprPtr basicSimplifyIntegerPower(const Context& c, const AlgebraicExprPtr& base, const AlgebraicExprPtr& integerExponent);

AlgebraicExprPtr basicSimplifyPower(const Context& c, const AlgebraicExprPtr& exprPtr) {
	const auto e = static_cast<const PowerExpr*>(exprPtr.get());
	using enum AlgebraicExprType;
	auto base = basicSimplifiy(c, e->base);
	auto exponent = basicSimplifiy(c, e->exponent);
	if (base->isUndefined() || exponent->isUndefined()) {
		return c.makeUndefined();
	}

	auto isPositiveNumber = [](const AlgebraicExprPtr& exprPtr) {
		if (exprPtr->type == INTEGER) {
			return exprPtr->asInteger()->value > 0;
		}
		if (exprPtr->type == RATIONAL) {
			// Assuming simplified form.
			return exprPtr->asRational()->numerator > 0;
		}
		return false;
	};
	if (base->isIntegerValue(0)) {
		if (isPositiveNumber(exponent)) {
			return std::make_unique<IntegerExpr>(0);
		} else {
			// Could add the case of simplifying 0^0, but I would rather leave it unsimplified.
			return std::make_unique<PowerExpr>(std::move(base), std::move(exponent));
		}
	}
	if (base->isIntegerValue(1)) {
		// x is not undefined so x is complex
		// https://complex-analysis.com/content/power_function.html
		// 1^x = exp(x log(1)) = exp(x(Log(1) + 2n pi i)) = exp(x 2n pi i))
		// 1^x = e^(ln(0) * x) = e^(0 * x) = e^0 = 1
		return std::make_unique<IntegerExpr>(1);
	}
	if (exponent->isInteger()) {
		return basicSimplifyIntegerPower(c, std::move(base), std::move(exponent));
	}

	return std::make_unique<PowerExpr>(std::move(base), std::move(exponent));
}

/*
simplifySum
let S = [s0, ..., sn] be the summands

if undefined in S -> undefined
if S = [s0] -> s0
let R = simplifySumRecursive(S)
if R = [] -> Integer(0)
if R = [r0] -> r0
else -> Sum(R)

simplifySumRecursive
let S = [s0, ..., sn] be the input

// The sum has to have >= 2 elements when called from simplifySum
if S = [s0, s1] {
	if s0.isSum() || s1.isSum() {
		merge sums
	}
	if s0.isConstant() && s1.isConstant() {
		sum = s0 + s1
		if sum == 0 -> []
		else [sum]
	}
	if s0.isIntegerValue(0) {
		-> [s1]
	}
	if s1.isIntegerValue(0) {
		-> [s0]
	}

	[s0Const, s0NonConst] = algebraicExprIntoConstAndNonConstFactor(s0)
	[s1Const, s1NonConst] = algebraicExprIntoConstAndNonConstFactor(s1)
	if s0NonConst == s1NonConst {
		combinedConstant = simplifySum(s0Const + s1Const)
		// This can't be simplifyProductRecursive because the constant might be simplifed to 0
		product = simplifyProduct([combinedConstant, s0NonConst])
		if product == 0 {
			-> []
		} else {
			-> [product]
		}

	}
	if s1 < s0 {
		-> [s1, s0]
	}
	-> S
} else {

}

I think the merging code might be possible to implement without so much recursion.
It basically gets 2 sorted lists of already simplified factors as an input and sorts them.

The sorting works like this
for example
ead * bfc
first both are sorted recursively
ade * bcf
Then it compares the first operands or simplifies them. In this case we have no simplification and the order is
ab so then it calls recursively
a ++ {
	de * bcf ->
	b ++ {
		de * cf ->
		c ++ {
			de * f ->
			d -> {
				e * f -> ef
			}
		}
	}
}
Where * stands for merging the products.
and ++ for prepending to the list.

ed * cf

One way for doing merging could be like this begin with 2 lists a and b
i64 aIndex = 0;
i64 bIndex = 0;
auto a = std::move(a[aIndex])
auto b = std::move(b[bIndex])
while (aIndex < a.size() && bIndex < b.size()) {
	auto simplifiedMultiplication = simplifyMultiplication(a, b)
	if (simplifiedMultiplication.size() == 1) {
		result.push_back(std::move(simplifiedMultiplication[0]))
		aIndex++;
		bIndex++;
		a = std::move(a[aIndex]);
		b = std::move(b[aIndex]);
	} else {
		// size == 2
		if (simplification swapped // TODO: Give some way to check it, can't do it know because it's moved out) {
			result.push_back(simplifiedMultiplication[0]) // b
			a = std::move(simplifiedMultiplication[1]) // a
			bIndex++
			b = std::move(b[bIndex]);
		} else {
			result.push_back(simplifiedMultiplication[0]) // a
			b = std::move(simplifiedMultiplication[1]) // b
			aIndex++
			a = std::move(a[aIndex]);
		}
	}
	// One of these loops won't execute.
	for (i32 i = aIndex; i < a.size(); i++) {
		result.push_back(std::move(a[i]));
	}
	for (i32 i = bIndex; i < b.size(); i++) {
		result.push_back(std::move(b[i]));
	}
}




*/
AlgebraicExprList basicSimplifySumRecursive(const Context& c, View<const AlgebraicExprPtr> factors);
AlgebraicExprList mergeSums(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b);
AlgebraicExprPtr basicSimplifySum(const Context& c, const AlgebraicExprList& summands) {
	if (summands.size() == 0) {
		CHECK_NOT_REACHED();
		return c.makeUndefined();
	}

	AlgebraicExprList simplifiedSummands;
	for (const auto& summand : summands) {
		simplifiedSummands.push_back(basicSimplifiy(c, summand));
	}

	for (const auto& summand : simplifiedSummands) {
		if (summand->isUndefined()) {
			return c.makeUndefined();
		}
	}

	if (simplifiedSummands.size() == 1) {
		return std::move(simplifiedSummands[0]);
	}

	auto moreSimplifiedSummands = basicSimplifySumRecursive(c, constView(simplifiedSummands));
	if (moreSimplifiedSummands.size() == 0) {
		return std::make_unique<IntegerExpr>(0);
	}
	if (moreSimplifiedSummands.size() == 1) {
		return std::move(moreSimplifiedSummands[0]);
	}
	return std::make_unique<SumExpr>(std::move(moreSimplifiedSummands));
}

struct ConstantFactorDeomposition {
	AlgebraicExpr constantFactor;
	AlgebraicExpr nonConstantFactor;
};

bool isConstant(const AlgebraicExprPtr& expr) {
	return expr->isInteger() || expr->isRational();
}

std::optional<AlgebraicExprPtr> algebraicExprConstantFactor(const Context& c, const AlgebraicExprPtr& expr) {
	switch (expr.get()->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return std::nullopt;

	case SYMBOL:
	case FUNCTION:
	case DERIVATIVE:
	case SUM:
	case POWER:
		return std::make_unique<IntegerExpr>(1);

	case PRODUCT: {
		const auto& e = expr->asProduct();
		// This is just used in the testing function.
		if (e->factors.size() < 1) {
			return algebraicExprClone(c, expr);
		}
		if (e->factors.size() < 2) {
			return algebraicExprClone(c, e->factors[0]);
		}
		if (isConstant(e->factors[0])) {
			// Simplified products always have the constant as the first factor.
			return algebraicExprClone(c, e->factors[0]);
		} else {
			return std::make_unique<IntegerExpr>(1);
		}
	}
	}
	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

std::optional<AlgebraicExprPtr> algebraicExprNonConstantFactor(const Context& c, const AlgebraicExprPtr& expr) {
	switch (expr.get()->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return std::nullopt;

	case SYMBOL:
	case FUNCTION:
	case DERIVATIVE:
	case SUM:
	case POWER:
		return algebraicExprClone(c, expr);

	case PRODUCT: {
		const auto& e = expr->asProduct();

		// This is just used in the testing function.
		if (e->factors.size() < 1) {
			return algebraicExprClone(c, expr);
		}
		if (e->factors.size() < 2) {
			return algebraicExprClone(c, e->factors[0]);
		}
		
		// Simplified products always have the constant as the first factor.
		if (isConstant(e->factors[0])) {
			if (e->factors.size() == 2) {
				return algebraicExprClone(c, e->factors[1]);
			}
			AlgebraicExprList factors;
			for (const auto& factor : rest(constView(e->factors))) {
				factors.push_back(algebraicExprClone(c, factor));
			}
			return std::make_unique<ProductExpr>(std::move(factors));
			// Simplified products always have the constant as the first factor.
			return algebraicExprClone(c, e->factors[0]);
		} else {
			return algebraicExprClone(c, expr);
		}
	}
	}
	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

AlgebraicExprList basicSimplifySumRecursive(const Context& c, View<const AlgebraicExprPtr> summands) {
	AlgebraicExprList simplifiedSummands;
	// It is assumed that summands.size() >= 2.
	if (summands.size() != 2) {
		const auto& first = summands[0];
		const auto rest = basicSimplifySumRecursive(c, ::rest(summands));
		if (first->isSum()) {
			return mergeSums(c, constView(first->asSum()->summands), constView(rest));
		} else {
			return mergeSums(c, constView(first), constView(rest));
		}
	}

	auto& a = summands[0];
	auto& b = summands[1];

	if (a->isSum() && b->isSum()) {
		return mergeSums(c, constView(a->asSum()->summands), constView(b->asSum()->summands));
	} 
	if (a->isSum()) {
		return mergeSums(c, constView(a->asSum()->summands), constView(b));
	}
	if (b->isSum()) {
		return mergeSums(c, constView(a), constView(b->asSum()->summands));
	}
	auto simplifyConstantSum = [&](
		IntegerType aNumerator, IntegerType aDenominator,
		IntegerType bNumerator, IntegerType bDenominator) -> void {

		IntegerType numerator = aNumerator * bDenominator + bNumerator * aDenominator;
		IntegerType denominator = aDenominator * bDenominator;
		const auto gcd = integerGcd(numerator, denominator);
		numerator /= gcd;
		denominator /= gcd;

		if (denominator == 1) {
			simplifiedSummands.push_back(std::make_unique<IntegerExpr>(numerator));
		} else {
			simplifiedSummands.push_back(std::make_unique<RationalExpr>(numerator, denominator));
		}
	};

	if (a->isInteger() && b->isInteger()) {
		simplifyConstantSum(
			a->asInteger()->value, 1,
			b->asInteger()->value, 1);
		return simplifiedSummands;
	}
	if (a->isInteger() && b->isRational()) {
		const auto bRational = b->asRational();
		simplifyConstantSum(
			a->asInteger()->value, 1,
			bRational->numerator, bRational->denominator);
		return simplifiedSummands;
	}
	if (a->isRational() && b->isInteger()) {
		const auto aRational = a->asRational();
		simplifyConstantSum(
			aRational->numerator, aRational->denominator,
			b->asInteger()->value, 1);
		return simplifiedSummands;
	}
	if (a->isRational() && b->isRational()) {
		const auto aRational = a->asRational();
		const auto bRational = b->asRational();
		simplifyConstantSum(
			aRational->numerator, aRational->denominator,
			bRational->numerator, bRational->denominator);
		return simplifiedSummands;
	}
	if (a->isIntegerValue(0)) {
		simplifiedSummands.push_back(algebraicExprClone(c, b));
		return simplifiedSummands;
	}
	if (b->isIntegerValue(0)) {
		simplifiedSummands.push_back(algebraicExprClone(c, a));
		return simplifiedSummands;
	}
	auto aNonConst = algebraicExprNonConstantFactor(c, a);
	auto bNonConst= algebraicExprNonConstantFactor(c, b);
	if (aNonConst.has_value() && bNonConst.has_value() && algebraicExprEquals(*aNonConst, *bNonConst)) {
		AlgebraicExprList summands;
		summands.push_back(*algebraicExprConstantFactor(c, a));
		summands.push_back(*algebraicExprConstantFactor(c, b));
		auto constantFactor = basicSimplifySum(c, summands);

		AlgebraicExprList product;
		product.push_back(std::move(constantFactor));
		product.push_back(std::move(*aNonConst));
		auto simplifiedProduct = basicSimplifyProduct(c, product);

		if (simplifiedProduct->isIntegerValue(0)) {
			return simplifiedSummands;
		}
		simplifiedSummands.push_back(std::move(simplifiedProduct));
		return simplifiedSummands;
	}
	if (algebraicExprLessThan(c, b, a)) {
		simplifiedSummands.push_back(algebraicExprClone(c, b));
		simplifiedSummands.push_back(algebraicExprClone(c, a));
		return simplifiedSummands;
	}

	simplifiedSummands.push_back(algebraicExprClone(c, a));
	simplifiedSummands.push_back(algebraicExprClone(c, b));
	return simplifiedSummands;
}

AlgebraicExprList algebraicExprListJoin(AlgebraicExprPtr&& first, AlgebraicExprList&& rest) {
	AlgebraicExprList result;
	result.push_back(std::move(first));
	for (auto& expr : rest) {
		result.push_back(std::move(expr));
	}
	return result;
}

AlgebraicExprList mergeSums(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	AlgebraicExprList result;
	if (a.size() == 0) {
		for (const auto& summand : b) {
			result.push_back(algebraicExprClone(c, summand));
		}
		return result;
	}
	if (b.size() == 0) {
		for (const auto& summand : a) {
			result.push_back(algebraicExprClone(c, summand));
		}
		return result;
	}
	result.push_back(algebraicExprClone(c, a[0]));
	result.push_back(algebraicExprClone(c, b[0]));
	// This is just the case of basicSimplifySumRecursive with 2 inputs. So could make a function for that.
	auto simplified = basicSimplifySumRecursive(c, constView(result));
	if (simplified.size() == 0) {
		return mergeSums(c, rest(a), rest(b));
	}
	
	if (simplified.size() == 1) {
		auto restMerged = mergeSums(c, rest(a), rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(restMerged));
	}
	// If the execution reached here simplified is either [a[0], b[0]] or [b[0] a[0]].
	if (algebraicExprEquals(simplified[0], a[0])) {
		// Not swapped.
		auto merged = mergeSums(c, rest(a), b);
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	} else {
		// Swapped
		auto merged = mergeSums(c, a, rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	}
}

AlgebraicExprList basicSimplifyProductRecursive(const Context& c, View<const AlgebraicExprPtr> factors);
AlgebraicExprList mergeProducts(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b);

AlgebraicExprList mergeProductsHelper(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	AlgebraicExprList result;
	if (a.size() == 0) {
		for (const auto& factor : b) {
			result.push_back(algebraicExprClone(c, factor));
		}
		return result;
	} 
	if (b.size() == 0) {
		for (const auto& factor : a) {
			result.push_back(algebraicExprClone(c, factor));
		}
		return result;
	}
	result.push_back(algebraicExprClone(c, a[0]));
	result.push_back(algebraicExprClone(c, b[0]));
	// This is only executed the case of basicSimplify with 2 inputs. 
	auto simplified = basicSimplifyProductRecursive(c, constView(result));
	if (simplified.size() == 0) {
		return mergeProducts(c, rest(a), rest(b));
	} 
	if (simplified.size() == 1) {
		auto restMerged = mergeProducts(c, rest(a), rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(restMerged));
	} 
	// Not swapped.
	// If the execution reached here simplified is either [a[0], b[0]] or [b[0] a[0]].
	if (algebraicExprEquals(simplified[0], a[0])) {
		auto merged = mergeProducts(c, rest(a), b);
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	} else {
		auto merged = mergeProducts(c, a, rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	}
};

//#define MERGE_PRODUCTS_DEBUG_PRINT

AlgebraicExprList mergeProducts(const Context& c, View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	#ifdef MERGE_PRODUCTS_DEBUG_PRINT
		put("mergeProducts");
		putnn("a = ");
		printAlgebraicExprView(a);
		put("");
		putnn("b = ");
		printAlgebraicExprView(b);
		put("");
	#endif
	auto result = mergeProductsHelper(c, a, b);
	#ifdef MERGE_PRODUCTS_DEBUG_PRINT
		putnn("mergeProducts result = ");
		debugPrintAlgebraicExprList(result);
	#endif
	return result;
}

// This returns an empty list if the result is just one, because it would need to be removed during merging anyway.
AlgebraicExprList basicSimplifyProductRecursive(const Context& c, View<const AlgebraicExprPtr> factors) {
	AlgebraicExprList simplifiedFactors;
	// It is assumed that factors.size() >= 2.
	if (factors.size() != 2) {
		const auto& first = factors[0];
		// This recursion could probably be converted into a loop.
		// It would just call merge the left product with a right one and assign it to some variable left
		// then again it would merge the left one with the next right one.
		// This do the base case of 2 iteratively instead of recursively.
		// mergeProducs expects 2 already simplified products.
		const auto rest = basicSimplifyProductRecursive(c, ::rest(factors));
		if (first->isProduct()) {
			return mergeProducts(c, constView(first->asProduct()->factors), constView(rest));
		} else {
			return mergeProducts(c, constView(first), constView(rest));
		}
	}
	// Could try to first collapse the tree and then sort. One thing is that transformations like 2(x + y) -> 2x + 2y might be affected by the order in which things are applied.
	// Also not sure which sorting algorithm would work. This isn't regular sorting, because you also have to combine things in order.

	auto& a = factors[0];
	auto& b = factors[1];

	// This cases can't be true when called from mergeProducts, because they arguments with which it calls are already simplified.
	if (a->isProduct() && b->isProduct()) {
		return mergeProducts(c, constView(a->asProduct()->factors), constView(b->asProduct()->factors));
	} 
	if (a->isProduct()) {
		return mergeProducts(c, constView(a->asProduct()->factors), constView(b));
	}
	if (b->isProduct()) {
		return mergeProducts(c, constView(a), constView(b->asProduct()->factors));
	}
	auto simplifyConstantProduct = [&](
		IntegerType aNumerator, IntegerType aDenominator,
		IntegerType bNumerator, IntegerType bDenominator) -> void {

		IntegerType numerator = aNumerator * bNumerator;
		IntegerType denominator = aDenominator * bDenominator;
		const auto gcd = integerGcd(numerator, denominator);
		numerator /= gcd;
		denominator /= gcd;

		if (denominator == 1) {
			simplifiedFactors.push_back(std::make_unique<IntegerExpr>(numerator));
		} else {
			simplifiedFactors.push_back(std::make_unique<RationalExpr>(numerator, denominator));
		}
	};

	if (a->isInteger() && b->isInteger()) {
		simplifyConstantProduct(
			a->asInteger()->value, 1,
			b->asInteger()->value, 1);
		return simplifiedFactors;
	}
	if (a->isInteger() && b->isRational()) {
		const auto bRational = b->asRational();
		simplifyConstantProduct(
			a->asInteger()->value, 1,
			bRational->numerator, bRational->denominator);
		return simplifiedFactors;
	}
	if (a->isRational() && b->isInteger()) {
		const auto aRational = a->asRational();
		simplifyConstantProduct(
			aRational->numerator, aRational->denominator,
			b->asInteger()->value, 1);
		return simplifiedFactors;
	}
	if (a->isRational() && b->isRational()) {
		const auto aRational = a->asRational();
		const auto bRational = b->asRational();
		simplifyConstantProduct(
			aRational->numerator, aRational->denominator,
			bRational->numerator, bRational->denominator);
		return simplifiedFactors;
	}
	if (a->isIntegerValue(1)) {
		simplifiedFactors.push_back(algebraicExprClone(c, b));
		return simplifiedFactors;
	}
	if (b->isIntegerValue(1)) {
		simplifiedFactors.push_back(algebraicExprClone(c, a));
		return simplifiedFactors;
	}
	const auto aBase = algebraicExprBase(c, a);
	const auto bBase = algebraicExprBase(c, b);
	if (aBase.has_value() && bBase.has_value() && algebraicExprEquals(*aBase, *bBase)) {
		AlgebraicExprList summands;
		summands.push_back(*algebraicExprExponent(c, a));
		summands.push_back(*algebraicExprExponent(c, b));
		auto exponent = basicSimplifySum(c, summands);

		auto simplifiedPower = basicSimplifyPower(c, std::make_unique<PowerExpr>(
			algebraicExprClone(c, *aBase),
			std::move(exponent)
		));

		if (simplifiedPower->isIntegerValue(1)) {
			return simplifiedFactors;
		}
		simplifiedFactors.push_back(std::move(simplifiedPower));
		return simplifiedFactors;
	}
	if (algebraicExprLessThan(c, b, a)) {
		simplifiedFactors.push_back(algebraicExprClone(c, b));
		simplifiedFactors.push_back(algebraicExprClone(c, a));
		return simplifiedFactors;
	}

	simplifiedFactors.push_back(algebraicExprClone(c, a));
	simplifiedFactors.push_back(algebraicExprClone(c, b));
	return simplifiedFactors;
}

/*
Simplifying of 
[e, a, d] * [b, f, c]
First simplify the list which just result in 
merge([a, d, e], [b, c, f]) -> 
simplifyRecursive([a, b])
*/
AlgebraicExprPtr basicSimplifyProduct(const Context& c, const AlgebraicExprList& factors) {
	if (factors.size() == 0) {
		CHECK_NOT_REACHED();
		return c.makeUndefined();
	}

	AlgebraicExprList simplifiedFactors;
	for (const auto& factor : factors) {
		simplifiedFactors.push_back(basicSimplifiy(c, factor));
	}

	for (const auto& factor : simplifiedFactors) {
		if (factor->isUndefined()) {
			return c.makeUndefined();
		}
	}
	for (const auto& factor : simplifiedFactors) {
		if (factor->isIntegerValue(0)) {
			return std::make_unique<IntegerExpr>(0);
		}
	}

	if (simplifiedFactors.size() == 1) {
		return std::move(simplifiedFactors[0]);
	}

	auto simplifiedProduct = basicSimplifyProductRecursive(c, constView(simplifiedFactors));
	if (simplifiedProduct.size() == 0) {
		return std::make_unique<IntegerExpr>(1);
	} 
	if (simplifiedProduct.size() == 1) {
		return std::move(simplifiedProduct[0]);
	} 
	if (simplifiedProduct.size() == 2 && isConstant(simplifiedProduct[0]) && simplifiedProduct[1]->isSum()) {
		// This does the simplification 2(x + y) -> 2x + 2y. 
		// Not doing this in general, because it would undo the factoring done in the simplification of sums.'
		// This is needed to simplify thing like
		// x + y + 2(x + y)
		// This isn't simplified normally because the addition simplification doesn't recognize x + y.
		auto& constant = simplifiedProduct[0]; 
		auto sum = simplifiedProduct[1]->asSum();
		AlgebraicExprList newSum;
		for (auto& summand : sum->summands) {
			AlgebraicExprList integerProduct;
			integerProduct.push_back(algebraicExprClone(c, constant));
			integerProduct.push_back(std::move(summand));
			// Could probably use basicSimplifyProductRecursive, but would need to handle the conversion of list to AlgebraicExpr or take that out of basicSimplifyProduct and put it here.
			//const auto simplifiedProduct = basicSimplifyProductRecursive(constView(factors));
			newSum.push_back(basicSimplifyProduct(c, integerProduct));
		}
		return basicSimplifySum(c, newSum);
	}
	return std::make_unique<ProductExpr>(std::move(simplifiedProduct));
}

//#define SIMPLIFY_INTEGER_POWER_DEBUG_PRINT

// Making the second argument a reference, because it doesn't always need to be moved, but when simplifiying a product it would need to be cloned many times.
AlgebraicExprPtr basicSimplifyIntegerPower(const Context& c, const AlgebraicExprPtr& base, const AlgebraicExprPtr& integerExponent) {
	#ifdef SIMPLIFY_INTEGER_POWER_DEBUG_PRINT
		put("basicSimplifyIntegerPower");
		putnn("base = ");
		printAlgebraicExpr(base);
		putnn("integerExponent = ");
		printAlgebraicExpr(integerExponent);
	#endif


	auto fractionToPower = [](IntegerType numerator, IntegerType denominator, IntegerType exponent) -> AlgebraicExprPtr {
		rationalNumberInCanonicalFormToIntegerPower(numerator, denominator, exponent);
		if (denominator == 1) {
			return std::make_unique<IntegerExpr>(numerator);
		} else {
			return std::make_unique<RationalExpr>(numerator, denominator);
		}
	};

	const auto& exponentInteger = integerExponent->asInteger()->value;
	if (base->isInteger()) {
		return fractionToPower(base->asInteger()->value, 1, exponentInteger);
	} 
	if (base->isRational()) {
		const auto baseRational = base->asRational();
		return fractionToPower(baseRational->numerator, baseRational->denominator, exponentInteger);
	}
	if (exponentInteger == 0) {
		// This is needed for things like aa^(-1) -> a^0 -> 1
		return std::make_unique<IntegerExpr>(1);
	}
	if (exponentInteger == 1) {
		return algebraicExprClone(c, base);
	}
	if (base->isPower()) {
		// (basePower.base^basePower.exponent)^integerExponent = 
		// basePower.base^(basePower.exponent * integerExponent)
		// This can change the domain. For exmple (x^(1/2))^2 -> x
		auto basePower = base->asPower();
		AlgebraicExprList product;
		product.push_back(std::move(basePower->exponent));
		product.push_back(algebraicExprClone(c, integerExponent));
		auto simplifiedProduct = basicSimplifyProduct(c, std::move(product));
		if (simplifiedProduct->isInteger()) {
			return basicSimplifyIntegerPower(c, std::move(basePower->base), std::move(simplifiedProduct));
		} else {
			return std::make_unique<PowerExpr>(std::move(basePower->base), std::move(simplifiedProduct));
		}
	}
	if (base->isProduct()) {
		auto baseProduct = base->asProduct();
		AlgebraicExprList simplifiedProduct;
		// (v1 ... vn)^m = v1^m ... vn^m
		for (auto& factor : baseProduct->factors) {
			simplifiedProduct.push_back(basicSimplifyIntegerPower(c, std::move(factor), integerExponent));
		}
		// Doing product simplification, because for example
		// ((xy)^(1/2) * z^2)^2 would be simplified by the above loop to
		// (xy)z^4 which has a nested product.
		return basicSimplifyProduct(c, std::move(simplifiedProduct));
	}

	return std::make_unique<PowerExpr>(algebraicExprClone(c, base), algebraicExprClone(c, integerExponent));
}

#include "ConstructionHelpers.hpp"

AlgebraicExprPtr Algebra::basicSimplifiy(const Context& c, const AlgebraicExprPtr& exprPtr) {
	using namespace AlgebraConstuctionHelpers;
	const auto expr = exprPtr.get();
	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER: {
		const auto e = static_cast<const IntegerExpr*>(expr);
		return std::make_unique<IntegerExpr>(e->value);
	}
	case RATIONAL: {
		const auto e = static_cast<const RationalExpr*>(expr);
		IntegerType numerator = e->numerator;
		IntegerType denominator = e->denominator;

		if (denominator == 0) {
			return c.makeUndefined();
		}
		if (denominator < 0) {
			denominator = -denominator;
			numerator = -numerator;
		}
		if (numerator == 0) {
			return std::make_unique<IntegerExpr>(0);
		}
		if (denominator == 1) {
			return std::make_unique<IntegerExpr>(e->numerator);
		}
		const auto gcd = integerGcd(denominator, numerator);
		ASSERT(gcd != 0);
		return std::make_unique<RationalExpr>(numerator / gcd, denominator / gcd);
	}
	case SYMBOL: {
		const auto e = static_cast<const SymbolExpr*>(expr);
		return std::make_unique<SymbolExpr>(e->symbol);
	}
	case FUNCTION: {
		const auto e = expr->asFunction();
		AlgebraicExprList arguments;
		for (const auto& argument : e->arguments) {
			arguments.push_back(basicSimplifiy(c, argument));
		}
		switch (e->function->type) {
			using enum FunctionType;
			case SIN:
			case COS:
				break;
			case LN: {
				if (arguments.size() != 1) {
					ASSERT_NOT_REACHED();
					break;
				}
				const auto& arg = arguments[0];
				if (arg->isSymbol() && arg->asSymbol()->symbol->type == SymbolType::E) {
					return integer(1);
				}
				break;
			}

		}
		return std::make_unique<FunctionExpr>(e->function, std::move(arguments));
	}
	case SUM: {
		return basicSimplifySum(c, static_cast<const SumExpr*>(expr)->summands);
	}
	case PRODUCT: {
		return basicSimplifyProduct(c, static_cast<const ProductExpr*>(expr)->factors);
	}
	case POWER: {
		return basicSimplifyPower(c, exprPtr);
	}
	case DERIVATIVE: {
		// TODO: Maybe just call derivative.
		// Also collapsing multiple derivatives will need to be handled.
		return algebraicExprClone(c, exprPtr);
	}
	}

	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

bool isSimplifiedExprList(const Context& c, const AlgebraicExprList& list, AlgebraicExprType listType) {
	if (list.size() < 2) {
		return false;
	}
	bool hasConstant = false;
	for (const auto& e : list) {
		if (!isSimplifiedExpr(c, e)) {
			return false;
		}
		if (e->type == listType) {
			// SR2 - nested sums and products not allowed
			return false;
		}
		if (isConstant(e)) {
			// SR1 - a simplified expression can only have one constant.
			if (hasConstant) {
				return false;
			}
			hasConstant = true;
		}
	}

	for (i32 i = 1; i < list.size(); i++) {
		const auto& a = list[i - 1];
		const auto& b = list[i];
		if (!algebraicExprLessThan(c, a, b)) {
			return false;
		}
	}

	return true;
}

bool Algebra::isSimplifiedExpr(const Context& c, const AlgebraicExprPtr& expr) {
	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER: return true;
	case RATIONAL: {
		const auto e = expr->asRational();
		return isRationalNumberInCanonicalForm(e->numerator, e->denominator);
	}
	case SYMBOL: {
		const auto e = expr->asSymbol();
		if (e->isUndefined()) {
			return false;
		}
		return true;
	}
	case FUNCTION: {
		const auto e = expr->asFunction();
		for (const auto& a : e->arguments) {
			if (!isSimplifiedExpr(c, a)) {
				return false;
			}
		}
		return true;
	}
	case SUM: {
		const auto e = expr->asSum();
		if (!isSimplifiedExprList(c, e->summands, e->type)) {
			return false;
		}

		for (i32 i = 0; i < e->summands.size(); i++) {
			const auto& a = e->summands[i];
			for (i32 j = i + 1; j < e->summands.size(); j++) {
				const auto& b = e->summands[j];
				const auto aTerm = algebraicExprNonConstantFactor(c, a);
				if (!aTerm.has_value()) {
					continue;
				}
				const auto bTerm = algebraicExprConstantFactor(c, b);
				if (!bTerm.has_value()) {
					continue;
				}
				if (algebraicExprEquals(*aTerm, *bTerm)) {
					// SR4 - sums with common terms are combined.
					return false;
				}
			}
		}
		return true;
	}
	case PRODUCT: {
		const auto e = expr->asProduct();
		if (!isSimplifiedExprList(c, e->factors, e->type)) {
			return false;
		}
		for (i32 i = 0; i < e->factors.size(); i++) {
			const auto& a = e->factors[i];
			for (i32 j = i + 1; j < e->factors.size(); j++) {
				const auto& b = e->factors[j];
				const auto aBase = algebraicExprBase(c, a);
				if (!aBase.has_value()) {
					continue;
				}
				const auto bBase = algebraicExprBase(c, b);
				if (!bBase.has_value()) {
					continue;
				}
				if (algebraicExprEquals(*aBase, *bBase)) {
					// SR3 - products with common factors must be combined
					return false;
				}
			}
		}
		return true;
	}
	case POWER: {
		const auto e = expr->asPower(); 
		const auto& b = e->base;
		const auto& p = e->exponent;
		if (!isSimplifiedExpr(c, e->exponent)) {
			return false;
		}
		if (!isSimplifiedExpr(c, e->base)) {
			return false;
		}
		if (e->exponent->isIntegerValue(0) || e->exponent->isIntegerValue(1)) {
			return false;
		}
		if (e->base->isInteger()) {
			if (!(b->isSymbol() || b->isSum() || b->isFunction() || b->isDerivative())) {
				return false;
			}
		} else {
			if (b->isIntegerValue(0) || b->isIntegerValue(1)) {
				return false;
			}
		}
		return true;
	}
	case DERIVATIVE: {
		const auto e = expr->asDerivative();
		return isSimplifiedExpr(c, e->expr);
	}
	}
	ASSERT_NOT_REACHED();
	return false;
}