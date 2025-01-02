#include "Simplification.hpp"
#include "../Arithmetic.hpp"
#include <Put.hpp>
#include "PrintExpr.hpp"
#include <View.hpp>
#include <Assertions.hpp>
#include <optional>

using namespace Algebra;

View<const AlgebraicExprPtr> rest(View<const AlgebraicExprPtr> view) {
	return View<const AlgebraicExprPtr>(view.data() + 1, view.size() - 1);
};

AlgebraicExprPtr basicSimplifiyProduct(const AlgebraicExprList& factors);

std::optional<AlgebraicExprPtr> algebraicExprBase(const AlgebraicExprPtr& exprPtr) {
	switch (exprPtr->type) {
		using enum AlgebraicExprType;
		case INTEGER:
		case RATIONAL:
			return std::nullopt;

		case SYMBOL:
		case FUNCTION:
		case SUM:
		case PRODUCT:
			return algebraicExprClone(exprPtr);

		case POWER: {
			const auto e = static_cast<const PowerExpr*>(exprPtr.get());
			// @Performance
			return algebraicExprClone(e->base);
		}
	}
	ASSERT_NOT_REACHED();
	return makeUndefined();
}

std::optional<AlgebraicExprPtr> algebraicExprExponent(const AlgebraicExprPtr& exprPtr) {
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
		return std::make_unique<IntegerExpr>(1);
	case POWER: {
		const auto e = static_cast<const PowerExpr*>(exprPtr.get());
		return algebraicExprClone(e->exponent);
	}
	}
	ASSERT_NOT_REACHED();
	return makeUndefined();
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
		return a->name == b->name;
	}
	case FUNCTION: {
		const auto a = static_cast<const FunctionExpr*>(aExpr);
		const auto b = static_cast<const FunctionExpr*>(bExpr);
		if (a->name != b->name) {
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
	}

	CHECK_NOT_REACHED();
	return false;
}

bool algebraicExprViewLessThan(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	for (i32 i = 0; ; i++) {
		const auto aIndex = i64(a.size()) - 1 - i;
		const auto bIndex = i64(b.size()) - 1 - i;
		if (aIndex < 0 || bIndex < 0) {
			break;
		}
		if (!algebraicExprEquals(a[aIndex], b[bIndex])) {
			return algebraicExprLessThan(a[aIndex], b[bIndex]);
		}
	}
	return a.size() < b.size();
}

bool algebraicExprListLessThan(const AlgebraicExprList& a, const AlgebraicExprList& b) {
	return algebraicExprViewLessThan(constView(a), constView(b));
}

bool algebraicExprPowerLessThan(const AlgebraicExprPtr& aBase, const AlgebraicExprPtr& aExponent,
	const AlgebraicExprPtr& bBase, const AlgebraicExprPtr& bExponent) {
	if (algebraicExprEquals(aBase, bBase)) {
		return algebraicExprLessThan(aExponent, bExponent);
	} else {
		return algebraicExprLessThan(aBase, bBase);
	}
}

// To make things more explicit could have each comparasion to have a separate function and return the function or it's negation.
bool Algebra::algebraicExprLessThan(const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr) {
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
		case FUNCTION:
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
		case FUNCTION:
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
			return a->name < b->name;
		}
		case INTEGER: 
		case RATIONAL: 
		case FUNCTION:
		case SUM:
		case PRODUCT:
		case POWER:
			break;
		}
		break;
	}
	case FUNCTION: {
		const auto a = static_cast<const FunctionExpr*>(aExpr);
		switch (bExpr->type) {
		case SYMBOL: {
			const auto b = static_cast<const SymbolExpr*>(bExpr);
			if (a->name == b->name) {
				// Put variables on the left of functions
				return false;
			} else {
				return a->name < b->name;
			}
		}
		case FUNCTION: {
			const auto b = static_cast<const FunctionExpr*>(bExpr);
			if (a->name == b->name) {
				return algebraicExprListLessThan(a->arguments, b->arguments);
			} else {
				return a->name < b->name;
			}
		}
		case INTEGER:
		case RATIONAL:
		case SUM:
		case PRODUCT:
		case POWER:
			break;
		}
		break;
	}
	case SUM: {
		const auto a = static_cast<const SumExpr*>(aExpr);
		switch (bExpr->type) {
		case SUM: {
			const auto b = static_cast<const SumExpr*>(bExpr);
			return algebraicExprListLessThan(a->summands, b->summands);
		}
		case SYMBOL:
		case FUNCTION: 
			return algebraicExprViewLessThan(constView(a->summands), constView(bExprPtr));

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
			return algebraicExprListLessThan(a->factors, b->factors);
		}
		case SYMBOL:
		case FUNCTION:
		case SUM:
		case POWER:
			return algebraicExprViewLessThan(constView(a->factors), constView(bExprPtr));

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
			return algebraicExprPowerLessThan(a->base, a->exponent, b->base, b->exponent);
		}

		case SYMBOL:
		case FUNCTION:
		case SUM:
			// @Performance: allocating. Could preallocate some constants for things like this.
			return algebraicExprPowerLessThan(a->base, a->exponent, bExprPtr, std::make_unique<IntegerExpr>(1));

		case PRODUCT:
		case INTEGER:
		case RATIONAL:
			break;
		}
		break;
	}
	}

	return !algebraicExprLessThan(bExprPtr, aExprPtr);
}

AlgebraicExprPtr basicSimplifyIntegerPower(AlgebraicExprPtr&& base, const AlgebraicExprPtr& integerExponent);

AlgebraicExprPtr basicSimplifyPower(const AlgebraicExprPtr& exprPtr) {
	const auto e = static_cast<const PowerExpr*>(exprPtr.get());
	using enum AlgebraicExprType;
	auto base = basicSimplifiy(e->base);
	auto exponent = basicSimplifiy(e->exponent);
	if (base->isUndefined() || exponent->isUndefined()) {
		return makeUndefined();
	}

	auto isPositiveNumber = [](const AlgebraicExprPtr& exprPtr) {
		if (exprPtr->type == INTEGER) {
			return exprPtr->asInteger()->value > 0;
		}
		if (exprPtr->type == RATIONAL) {
			// Assuming simplified form.
			return exprPtr->asRational()->numerator > 0;
		}
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
		return basicSimplifyIntegerPower(std::move(base), std::move(exponent));
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


*/
AlgebraicExprList basicSimplifySumRecursive(View<const AlgebraicExprPtr> factors);
AlgebraicExprList mergeSums(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b);
AlgebraicExprPtr basicSimplifySum(const AlgebraicExprList& summands) {
	if (summands.size() == 0) {
		CHECK_NOT_REACHED();
		return makeUndefined();
	}

	AlgebraicExprList simplifiedSummands;
	for (const auto& summand : summands) {
		simplifiedSummands.push_back(basicSimplifiy(summand));
	}

	for (const auto& summand : simplifiedSummands) {
		if (summand->isUndefined()) {
			return makeUndefined();
		}
	}

	if (simplifiedSummands.size() == 1) {
		return std::move(simplifiedSummands[0]);
	}

	auto moreSimplifiedSummands = basicSimplifySumRecursive(constView(simplifiedSummands));
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

bool isConstant(AlgebraicExprPtr& expr) {
	return expr->isInteger() || expr->isRational();
}

std::optional<AlgebraicExprPtr> algebraicExprConstantFactor(const AlgebraicExprPtr& expr) {
	switch (expr.get()->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return std::nullopt;

	case SYMBOL:
	case FUNCTION:
	case SUM:
	case POWER:
		return std::make_unique<IntegerExpr>(1);

	case PRODUCT: {
		const auto& e = expr->asProduct();
		ASSERT(e->factors.size() >= 2);
		if (isConstant(e->factors[0])) {
			// Simplified products always have the constant as the first factor.
			return algebraicExprClone(e->factors[0]);
		} else {
			return std::make_unique<IntegerExpr>(1);
		}
	}
	}
	ASSERT_NOT_REACHED();
	return makeUndefined();
}

std::optional<AlgebraicExprPtr> algebraicExprNonConstantFactor(const AlgebraicExprPtr& expr) {
	switch (expr.get()->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return std::nullopt;

	case SYMBOL:
	case FUNCTION:
	case SUM:
	case POWER:
		return algebraicExprClone(expr);

	case PRODUCT: {
		const auto& e = expr->asProduct();
		ASSERT(e->factors.size() >= 2);
		// Simplified products always have the constant as the first factor.
		if (isConstant(e->factors[0])) {
			if (e->factors.size() == 2) {
				return algebraicExprClone(e->factors[1]);
			}
			AlgebraicExprList factors;
			for (const auto& factor : rest(constView(e->factors))) {
				factors.push_back(algebraicExprClone(factor));
			}
			return std::make_unique<ProductExpr>(std::move(factors));
			// Simplified products always have the constant as the first factor.
			return algebraicExprClone(e->factors[0]);
		} else {
			return algebraicExprClone(expr);
		}
	}
	}
	ASSERT_NOT_REACHED();
	return makeUndefined();
}

AlgebraicExprList basicSimplifySumRecursive(View<const AlgebraicExprPtr> summands) {
	AlgebraicExprList simplifiedSummands;
	// It is assumed that summands.size() >= 2.
	if (summands.size() != 2) {
		const auto& first = summands[0];
		const auto rest = basicSimplifySumRecursive(::rest(summands));
		if (first->isSum()) {
			return mergeSums(constView(first->asSum()->summands), constView(rest));
		} else {
			return mergeSums(constView(first), constView(rest));
		}
	}

	auto& a = summands[0];
	auto& b = summands[1];

	if (a->isSum() && b->isSum()) {
		return mergeSums(constView(a->asSum()->summands), constView(b->asSum()->summands));
	} 
	if (a->isSum()) {
		return mergeSums(constView(a->asSum()->summands), constView(b));
	}
	if (b->isSum()) {
		return mergeSums(constView(a), constView(b->asSum()->summands));
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
		simplifiedSummands.push_back(algebraicExprClone(b));
		return simplifiedSummands;
	}
	if (b->isIntegerValue(0)) {
		simplifiedSummands.push_back(algebraicExprClone(a));
		return simplifiedSummands;
	}
	auto aNonConst = algebraicExprNonConstantFactor(a);
	auto bNonConst= algebraicExprNonConstantFactor(b);
	if (aNonConst.has_value() && bNonConst.has_value() && algebraicExprEquals(*aNonConst, *bNonConst)) {
		AlgebraicExprList summands;
		summands.push_back(*algebraicExprConstantFactor(a));
		summands.push_back(*algebraicExprConstantFactor(b));
		auto constantFactor = basicSimplifySum(summands);

		AlgebraicExprList product;
		product.push_back(std::move(constantFactor));
		product.push_back(std::move(*aNonConst));
		auto simplifiedProduct = basicSimplifiyProduct(product);

		if (simplifiedProduct->isIntegerValue(0)) {
			return simplifiedSummands;
		}
		simplifiedSummands.push_back(std::move(simplifiedProduct));
		return simplifiedSummands;
	}
	if (algebraicExprLessThan(b, a)) {
		simplifiedSummands.push_back(algebraicExprClone(b));
		simplifiedSummands.push_back(algebraicExprClone(a));
		return simplifiedSummands;
	}

	simplifiedSummands.push_back(algebraicExprClone(a));
	simplifiedSummands.push_back(algebraicExprClone(b));
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

AlgebraicExprList mergeSums(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	AlgebraicExprList result;
	if (a.size() == 0) {
		for (const auto& summand : b) {
			result.push_back(algebraicExprClone(summand));
		}
		return result;
	}
	if (b.size() == 0) {
		for (const auto& summand : a) {
			result.push_back(algebraicExprClone(summand));
		}
		return result;
	}
	result.push_back(algebraicExprClone(a[0]));
	result.push_back(algebraicExprClone(b[0]));
	// This is just the case of basicSimplifySumRecursive with 2 inputs. So could make a function for that.
	auto simplified = basicSimplifySumRecursive(constView(result));
	if (simplified.size() == 0) {
		return mergeSums(rest(a), rest(b));
	}
	
	if (simplified.size() == 1) {
		auto restMerged = mergeSums(rest(a), rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(restMerged));
	}
	// If the execution reached here simplified is either [a[0], b[0]] or [b[0] a[0]].
	if (algebraicExprEquals(simplified[0], a[0])) {
		// Not swapped.
		auto merged = mergeSums(rest(a), b);
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	} else {
		// Swapped
		auto merged = mergeSums(a, rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	}
}

AlgebraicExprList basicSimplifyProductRecursive(View<const AlgebraicExprPtr> factors);
AlgebraicExprList mergeProducts(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b);

AlgebraicExprList mergeProductsHelper(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	AlgebraicExprList result;
	if (a.size() == 0) {
		for (const auto& factor : b) {
			result.push_back(algebraicExprClone(factor));
		}
		return result;
	} 
	if (b.size() == 0) {
		for (const auto& factor : a) {
			result.push_back(algebraicExprClone(factor));
		}
		return result;
	}
	result.push_back(algebraicExprClone(a[0]));
	result.push_back(algebraicExprClone(b[0]));
	// This is just the case of basicSimplify with 2 inputs. 
	auto simplified = basicSimplifyProductRecursive(constView(result));
	if (simplified.size() == 0) {
		return mergeProducts(rest(a), rest(b));
	} 
	if (simplified.size() == 1) {
		auto restMerged = mergeProducts(rest(a), rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(restMerged));
	} 
	// Not swapped.
	// If the execution reached here simplified is either [a[0], b[0]] or [b[0] a[0]].
	if (algebraicExprEquals(simplified[0], a[0])) {
		auto merged = mergeProducts(rest(a), b);
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	} else {
		auto merged = mergeProducts(a, rest(b));
		return algebraicExprListJoin(std::move(simplified[0]), std::move(merged));
	}
};

//#define MERGE_PRODUCTS_DEBUG_PRINT

AlgebraicExprList mergeProducts(View<const AlgebraicExprPtr> a, View<const AlgebraicExprPtr> b) {
	#ifdef MERGE_PRODUCTS_DEBUG_PRINT
		put("mergeProducts");
		putnn("a = ");
		printAlgebraicExprView(a);
		put("");
		putnn("b = ");
		printAlgebraicExprView(b);
		put("");
	#endif
	auto result = mergeProductsHelper(a, b);
	#ifdef MERGE_PRODUCTS_DEBUG_PRINT
		putnn("mergeProducts result = ");
		debugPrintAlgebraicExprList(result);
	#endif
	return result;
}

// This returns an empty list if the result is just one, because it would need to be removed during merging anyway.
AlgebraicExprList basicSimplifyProductRecursive(View<const AlgebraicExprPtr> factors) {
	AlgebraicExprList simplifiedFactors;
	// It is assumed that factors.size() >= 2.
	if (factors.size() != 2) {
		const auto& first = factors[0];
		const auto rest = basicSimplifyProductRecursive(::rest(factors));
		if (first->isProduct()) {
			return mergeProducts(constView(first->asProduct()->factors), constView(rest));
		} else {
			return mergeProducts(constView(first), constView(rest));
		}
	}

	auto& a = factors[0];
	auto& b = factors[1];

	if (a->isProduct() && b->isProduct()) {
		return mergeProducts(constView(a->asProduct()->factors), constView(b->asProduct()->factors));
	} 
	if (a->isProduct()) {
		return mergeProducts(constView(a->asProduct()->factors), constView(b));
	}
	if (b->isProduct()) {
		return mergeProducts(constView(a), constView(b->asProduct()->factors));
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
		simplifiedFactors.push_back(algebraicExprClone(b));
		return simplifiedFactors;
	}
	if (b->isIntegerValue(1)) {
		simplifiedFactors.push_back(algebraicExprClone(a));
		return simplifiedFactors;
	}
	const auto aBase = algebraicExprBase(a);
	const auto bBase = algebraicExprBase(b);
	if (aBase.has_value() && bBase.has_value() && algebraicExprEquals(*aBase, *bBase)) {
		AlgebraicExprList summands;
		summands.push_back(*algebraicExprExponent(a));
		summands.push_back(*algebraicExprExponent(b));
		auto exponent = basicSimplifySum(summands);

		auto simplifiedPower = basicSimplifyPower(std::make_unique<PowerExpr>(
			algebraicExprClone(*aBase),
			std::move(exponent)
		));

		if (simplifiedPower->isIntegerValue(1)) {
			return simplifiedFactors;
		}
		simplifiedFactors.push_back(std::move(simplifiedPower));
		return simplifiedFactors;
	}
	if (algebraicExprLessThan(b, a)) {
		simplifiedFactors.push_back(algebraicExprClone(b));
		simplifiedFactors.push_back(algebraicExprClone(a));
		return simplifiedFactors;
	}

	simplifiedFactors.push_back(algebraicExprClone(a));
	simplifiedFactors.push_back(algebraicExprClone(b));
	return simplifiedFactors;
}

AlgebraicExprPtr basicSimplifiyProduct(const AlgebraicExprList& factors) {
	if (factors.size() == 0) {
		CHECK_NOT_REACHED();
		return makeUndefined();
	}

	AlgebraicExprList simplifiedFactors;
	for (const auto& factor : factors) {
		simplifiedFactors.push_back(basicSimplifiy(factor));
	}

	for (const auto& factor : simplifiedFactors) {
		if (factor->isUndefined()) {
			return makeUndefined();
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

	auto moreSimplifiedFactors = basicSimplifyProductRecursive(constView(simplifiedFactors));
	if (moreSimplifiedFactors.size() == 0) {
		return std::make_unique<IntegerExpr>(1);
	} 
	if (moreSimplifiedFactors.size() == 1) {
		return std::move(moreSimplifiedFactors[0]);
	} 
	return std::make_unique<ProductExpr>(std::move(moreSimplifiedFactors));
}

//#define SIMPLIFY_INTEGER_POWER_DEBUG_PRINT

// Making the second argument a reference, because it doesn't always need to be moved, but when simplifiying a product it would need to be cloned many times.
AlgebraicExprPtr basicSimplifyIntegerPower(AlgebraicExprPtr&& base, const AlgebraicExprPtr& integerExponent) {
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
		return base;
	}
	if (base->isPower()) {
		// (basePower.base^basePower.exponent)^integerExponent = 
		// basePower.base^(basePower.exponent * integerExponent)
		// This can change the domain. For exmple (x^(1/2))^2 -> x
		auto basePower = base->asPower();
		AlgebraicExprList product;
		product.push_back(std::move(basePower->exponent));
		product.push_back(algebraicExprClone(integerExponent));
		auto simplifiedProduct = basicSimplifiyProduct(std::move(product));
		if (simplifiedProduct->isInteger()) {
			return basicSimplifyIntegerPower(std::move(basePower->base), std::move(simplifiedProduct));
		} else {
			return std::make_unique<PowerExpr>(std::move(basePower->base), std::move(simplifiedProduct));
		}
	}
	if (base->isProduct()) {
		auto baseProduct = base->asProduct();
		AlgebraicExprList simplifiedProduct;
		// (v1 ... vn)^m = v1^m ... vn^m
		for (auto& factor : baseProduct->factors) {
			simplifiedProduct.push_back(basicSimplifyIntegerPower(std::move(factor), integerExponent));
		}
		// Doing product simplification, because for example
		// ((xy)^(1/2) * z^2)^2 would be simplified by the above loop to
		// (xy)z^4 which has a nested product.
		return basicSimplifiyProduct(std::move(simplifiedProduct));
	}

	return std::make_unique<PowerExpr>(std::move(base), algebraicExprClone(integerExponent));
}

AlgebraicExprPtr Algebra::basicSimplifiy(const AlgebraicExprPtr& exprPtr) {
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
			return makeUndefined();
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
		return std::make_unique<SymbolExpr>(std::string(e->name));
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		AlgebraicExprList arguments;
		for (const auto& argument : e->arguments) {
			arguments.push_back(basicSimplifiy(argument));
		}
		return std::make_unique<FunctionExpr>(std::string(e->name), std::move(arguments));
	}
	case SUM: {
		return basicSimplifySum(static_cast<const SumExpr*>(expr)->summands);
	}
	case PRODUCT: {
		return basicSimplifiyProduct(static_cast<const ProductExpr*>(expr)->factors);
	}
	case POWER: {
		return basicSimplifyPower(exprPtr);
	}
	}

	ASSERT_NOT_REACHED();
	return makeUndefined();
}
