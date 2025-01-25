#include "Polynomial.hpp"
#include "Simplification.hpp"
#include "ConstructionHelpers.hpp"

using namespace AlgebraConstuctionHelpers;

bool isPolynomialConstant(const AlgebraicExprPtr& expr) {
	return expr->isInteger() || expr->isRational();
}

// x^2 + x is a first degree polynomial in { x, x^2 }
bool Algebra::isGeneralMonomial(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables) {
	if (anyAlgebraicExprEquals(expr, generalizedVariables)) {
		return true;
	}

	if (expr->isPower()) {
		const auto e = expr->asPower();
		if (anyAlgebraicExprEquals(e->base, generalizedVariables) 
			&& e->exponent->isInteger() && e->exponent->asInteger()->value > 1) {
			return true;
		}
	} else if (expr->isProduct()) {
		const auto e = expr->asProduct();
		for (const auto& f : e->factors) {
			if (!isGeneralMonomial(f, generalizedVariables)) {
				return false;
			}
		}
		return true;
	}

	// Checking this last to avoid redundant recursion.
	return algebraicExprIsFreeOfList(expr, generalizedVariables);
}

bool Algebra::isGeneralMonomial(const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable) {
	return isGeneralPolynomial(expr, constView(generalizedVariable));
}

bool Algebra::isGeneralPolynomial(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables) {
	if (expr->isSum()) {
		const auto e = expr->asSum();
		for (const auto& s : e->summands) {
			if (!isGeneralMonomial(s, generalizedVariables)) {
				return false;
			}
		}
		return true;
	}

	if (isGeneralMonomial(expr, generalizedVariables)) {
		return true;
	}
	// Checking this last to avoid redundant recursion.

	return false;
}

bool Algebra::isGeneralPolynomial(const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable) {
	return isGeneralPolynomial(expr, constView(generalizedVariable));
}

i32 Algebra::generalMonomialDegree(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables) {
	if (anyAlgebraicExprEquals(expr, generalizedVariables)) {
		return 1;
	}

	if (expr->isPower()) {
		const auto e = expr->asPower();
		if (anyAlgebraicExprEquals(e->base, generalizedVariables)
			&& e->exponent->isInteger() && e->exponent->asInteger()->value > 1) {
			return e->exponent->asInteger()->value;
		}
	} else if (expr->isProduct()) {
		const auto e = expr->asProduct();
		auto degreeSum = 0;
		for (const auto& f : e->factors) {
			const auto degree = generalMonomialDegree(f, generalizedVariables);
			if (degree == -1) {
				ASSERT_NOT_REACHED();
				return -1;
			}
			degreeSum += degree;
		}
		return degreeSum;
	}

	// Checking this last to avoid redundant recursion.
	if (algebraicExprIsFreeOfList(expr, generalizedVariables)) {
		// Assuming that the expr is fully simplified
		if (expr->isIntegerValue(0)) {
			return -1;
		} else {
			return 0;
		}
	} else {
		ASSERT_NOT_REACHED();
		return -1;
	}
}

i32 Algebra::generalPolynomialDegree(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables) {
	if (expr->isSum()) {
		i32 degree = -1;
		const auto e = expr->asSum();
		for (const auto& s : e->summands) {
			degree = std::max(degree, generalMonomialDegree(s, generalizedVariables));
		}
		return degree;
	}

	// Checking this last to avoid redundant recursion.
	return generalMonomialDegree(expr, generalizedVariables);
}

std::optional<MonomialCoefficent> Algebra::generalMonomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable) {
	
	if (algebraicExprEquals(expr, generalizedVariable)) {
		return MonomialCoefficent{ integer(1), 1 };
	}

	if (expr->isPower()) {
		const auto e = expr->asPower();
		if (algebraicExprEquals(e->base, generalizedVariable) && e->exponent->isInteger()) {
			const auto exponent = e->exponent->asInteger()->value;
			if (exponent > 1) {
				return MonomialCoefficent{ integer(1), i32(exponent) };
			}
		}
	} else if (expr->isProduct()) {
		const auto e = expr->asProduct();
		i32 degree = 0;
		AlgebraicExprList constantFactors;
		for (const auto& f : e->factors) {
			auto coefficient = generalMonomialCoefficient(c, f, generalizedVariable);
			if (!coefficient.has_value()) {
				return std::nullopt;
			}
			if (coefficient->degree == 0) {
				constantFactors.push_back(std::move(coefficient->coefficient));
			} else {
				if (degree != 0) {
					// If there are non combined products then it's not simplified.
					ASSERT_NOT_REACHED();
					return std::nullopt;
				}
				degree = coefficient->degree;
			}
		}
		return MonomialCoefficent{ tryProduct(std::move(constantFactors)), degree };
	} 
	if (algebraicExprIsFreeOf(expr, generalizedVariable)) {
		// What about negative? I don't think it is needed anywhere.
		return MonomialCoefficent{ algebraicExprClone(c, expr), 0 };
	}

	return std::nullopt;
}

std::optional<AlgebraicExprPtr> Algebra::generalPolynomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable, i32 power) {
	if (expr->isSum()) {
		if (algebraicExprEquals(expr, generalizedVariable)) {
			if (power == 1) {
				return integer(1);
			} else {
				return integer(0);
			}
		}
		const auto e = expr->asSum();
		AlgebraicExprList sum;
		for (const auto& s : e->summands) {
			auto coefficient = generalMonomialCoefficient(c, s, generalizedVariable);
			if (!coefficient.has_value()) {
				return std::nullopt;
			}
			if (coefficient->degree == power) {
				sum.push_back(std::move(coefficient->coefficient));
			}
		}
		return trySum(std::move(sum));
	} 

	auto coefficient = generalMonomialCoefficient(c, expr, generalizedVariable);
	if (!coefficient.has_value()) {
		return std::nullopt;
	}
	if (coefficient->degree != power) {
		return integer(0);
	}
	return std::move(coefficient->coefficient);
}
