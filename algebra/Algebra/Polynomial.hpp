#pragma once

#include "Expr.hpp"
#include <View.hpp>
#include <optional>

namespace Algebra {

// It is assumed that the inputs are simplified.
bool isGeneralMonomial(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables);
bool isGeneralMonomial(const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable);
bool isGeneralPolynomial(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables);
bool isGeneralPolynomial(const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable);

// It is assumed that expr is a general monomial
i32 generalMonomialDegree(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables);
i32 generalPolynomialDegree(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> generalizedVariables);

struct MonomialCoefficent {
	AlgebraicExprPtr coefficient;
	i32 degree;
};
// In polynomials there is a indepencene conditions that says the coefficients have to be independnt of the variable.
// This function takes only one variable so sometimes it can't detect the correct variables
// For example the expression expr = ax + bx^(1/2) + c is a degree 1 polynomial in the variables { x, x^(1/2) }, but because of the independence condition if you try to use generalPolynomialCoefficient(expr, x, 1)) you get std::nullopt, because generalMonomialCoefficient(bx^(1/2)) return std::nullopt, because the expression is not independent of x. To fix this I could pass more of the variables and check if the expression matches any of the variables. 
// You could try first getting the coefficient c of x^(1/2) then subtracting c x^(1/2) and then getting the coefficient related to x. This might not work, because simplification might not fully remove x^(1/2) from the expression. For example ax^(1/2) + bx^(1/2) - (a + b)x^(1/2) won't simplify to 0 with basic simplification.
// It is probably simplest to just use the coefficient function that doesn't require the independence condition.
std::optional<MonomialCoefficent> generalMonomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable);
std::optional<AlgebraicExprPtr> generalPolynomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable, i32 power);
}