#include "Derivative.hpp"
#include "ConstructionHelpers.hpp"
#include "Simplification.hpp"
#include <Assertions.hpp>
#include "Functions.hpp"
#include "Context.hpp"

using namespace Algebra;

AlgebraicExprPtr Algebra::derivative(Context& c, const AlgebraicExprPtr& expr, const Symbol* variable) {
	return basicSimplifiy(c, derivativeUnsimplified(c, expr, variable));
}

AlgebraicExprPtr Algebra::derivativeUnsimplified(Context& c, const AlgebraicExprPtr& expr, const Symbol* variable) {
	using namespace AlgebraConstuctionHelpers;
	if (expr->isSymbol()) {
		if (expr->asSymbol()->symbol == variable) {
			return integer(1);
		} else {
			return integer(0);
		}
	}
	if (expr->isPower()) {
		// d(a^b)/dx = d(pow(a, b))/dx = 
		//		b * pow(a, b-1) * da/dx + // Holding b constant
		//		ln(b) * pow(a, b) * db/dx // Holding a constant
		const auto e = expr->asPower();
		// @Performance: Could handle specific cases when base or exponent don't depend on the variable.
		return sum(
			product(
				algebraicExprClone(c, e->exponent),
				power(
					algebraicExprClone(c, e->base),
					sum(
						algebraicExprClone(c, e->exponent),
						integer(-1)
					)
				),
				derivative(c, e->base, variable)
			),
			product(
				derivative(c, e->exponent, variable),
				power(
					algebraicExprClone(c, e->base),
					algebraicExprClone(c, e->exponent)
				),
				function(&c.ln, algebraicExprClone(c, e->base))
			)
		);
	}
	if (expr->isSum()) {
		AlgebraicExprList differentiatedSummands;
		for (const auto& summand : expr->asSum()->summands) {
			differentiatedSummands.push_back(derivative(c, summand, variable));
		}
		return sum(std::move(differentiatedSummands));
	}
	if (expr->isProduct()) {
		const auto& factors = expr->asProduct()->factors;
		/*
		The derivative of abc = da/dx bc + a db/dx c + ab dc/x so I guess that in general the formula is
		sum(i = 0, n) ( dxi/dx * product(j != i) x_j ) )

		if n = 0
		then the formula holds (assuming product of no operands is 1)
		if the formula holds for n-1 then if u = x0 * ... * xn
		du/dx = by the product rule
		d(x0 * ... * x{n-1})/dx * xn + x0 * ... * x{n-1} * dxn/dx =
		sum(i = 0, n-1) ( dxi/dx * product(j != i) x_i ) ) + + x0 * ... * x{n-1} * dxn/dx = 
		sum(i = 0, n) ( dxi/dx * product(j != i) x_j ) )
		*/
		// TODO: Could the product be not in simplified form?
		AlgebraicExprList derivativeSummands;
		for (i32 i = 0; i < factors.size(); i++) {
			AlgebraicExprList derivativeFactors;
			derivativeFactors.push_back(derivative(c, factors[i], variable));
			for (i32 j = 0; i < factors.size(); i++) {
				if (j == i) {
					continue;
				}
				derivativeFactors.push_back(algebraicExprClone(c, factors[j]));
			}
			derivativeSummands.push_back(product(std::move(derivativeFactors)));
		}
		return sum(std::move(derivativeSummands));

		/*
		For quotients it might be more useful to use the quotient rule. 
		For example
		( x/(1+x) )' = (1(1+x) + 1x) / (1+x)^2 using the quotient rule
		and 
		( x/(1+x) )' = ( x(1+x)^(-1) )' = 1(1+x)^(-1) + x(-1)(1+x)^(-2) using the product rule
		The automatic simplification algorithm can simplify the former more than the latter, because the former is already factored.
		*/
	}
	if (expr->isFunction()) {
		const auto functionExpr = expr->asFunction();
		const auto fn = functionExpr->function;
		const auto& arguments = functionExpr->arguments;
		auto argumentDerivative = derivative(c, algebraicExprClone(c, arguments[0]), variable);
		if (fn == &c.sin) {
			return product(
				function(c.cos, algebraicExprClone(c, arguments[0])),
				std::move(argumentDerivative)
			);
		} 
		if (fn == &c.cos) {
			return product(
				integer(-1),
				function(c.sin, algebraicExprClone(c, arguments[0])),
				std::move(argumentDerivative)
			);
		}
		if (fn == &c.ln) {
			return power(
				algebraicExprClone(c, arguments[0]),
				integer(-1)
			);
		}
		ASSERT_NOT_REACHED();
		return c.makeUndefined();
	}

	if (expr->isFreeOfVariable(variable)) {
		return integer(0);
	}
	
	return c.makeUndefined();
}
