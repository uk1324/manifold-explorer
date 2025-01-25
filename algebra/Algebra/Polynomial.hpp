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
std::optional<MonomialCoefficent> generalMonomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable);
std::optional<AlgebraicExprPtr> generalPolynomialCoefficient(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& generalizedVariable, i32 power);
}