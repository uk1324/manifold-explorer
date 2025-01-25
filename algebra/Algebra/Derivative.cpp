#include "Derivative.hpp"
#include "ConstructionHelpers.hpp"
#include "Simplification.hpp"
#include <Assertions.hpp>
#include "Functions.hpp"
#include "Context.hpp"

using namespace Algebra;

AlgebraicExprPtr Algebra::derivative(const Context& c, const AlgebraicExprPtr& expr, const Symbol* variable) {
	return basicSimplifiy(c, derivativeUnsimplified(c, expr, variable));
}

/*
Initially I thought about making a derivative a new function. But it is probably better to derivatives of expressions rather than functions. If you just implemented derivatvies of functions that you couldn't for example have a derivative of an integral which can't always be evaluated. This would require having 2 different kinds of derivatives.
Also there would be an issue with naming the parameters to a function. Technically a function doesn't need to have any names for the inputs it could be just considered as a set of pairs of inputs and outputs. If expressions are used instead of functions then there is no such issue.
So in summary there is
Derivative(f(x), x) not Derivative(f, 0)(x). Where 0 is the argument index.
*/
AlgebraicExprPtr Algebra::derivativeUnsimplified(const Context& c, const AlgebraicExprPtr& expr, const Symbol* variable) {
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
		//		ln(a) * pow(a, b) * db/dx // Holding a constant
		// TODO: The derivative of 0^x is undefined for all x. 
		// 0^x = 
		//	if x = 0 then 1 
		//	else if x > 0 then 0
		//	else undefined
		// So the derivative should be defined on (0, +inf).
		// In sympy diff(Rational(0)**x, x) = nan
		// Wolfram alpha gives the correct values for (0^x)', but fails for ((sin(x)^2 + cos(x)^2 - 1)^x)'.
		// One way in which this is kind of true is that e^(-inf x) = e^(-inf) = 0. So according to this definition 0^x is zero if x > 0 and undefined if x <= 0. Depending on how you define 0 * inf. 

		/*
		expr can't be equal to x^0 because the expression is in simplified form. TODO: Write which rule this uses.
		Also the simplified version of 0 * x^(0 - 1) is zero. The only issue is that the domain changes.
		*/
		const auto e = expr->asPower();
		// @Performance: Could handle specific cases when base or exponent don't depend on the variable. This might lead to more code execution, but would do less allocation.
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
			for (i32 j = 0; j < factors.size(); j++) {
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
	if (expr->isFunction() && expr->asFunction()->function->arity == 1) {
		const auto functionExpr = expr->asFunction();
		const auto fn = functionExpr->function;
		const auto& arguments = functionExpr->arguments;
		auto argumentDerivative = derivative(c, algebraicExprClone(c, arguments[0]), variable);
		auto multipliedByArgumentDerivative = [&](AlgebraicExprPtr&& expr) -> AlgebraicExprPtr {
			return product(std::move(expr), std::move(argumentDerivative));
		};
		switch (fn->type) {
			using enum FunctionType;
		case SIN: {
			return multipliedByArgumentDerivative(
				function(c.cos, algebraicExprClone(c, arguments[0]))
			);
		}
		case COS: {
			return product(
				integer(-1),
				function(c.sin, algebraicExprClone(c, arguments[0])),
				std::move(argumentDerivative)
			);
		}
		case LN: {
			return multipliedByArgumentDerivative(power(
				algebraicExprClone(c, arguments[0]),
				integer(-1)
			));
		}

		case ABS:
			break;

		case TAN: {
			return multipliedByArgumentDerivative(power(
				function(c.cos, algebraicExprClone(c, arguments[0])), 
				integer(-2)
			));
		}

		case ASIN: {
			return multipliedByArgumentDerivative(power(
				sum(
					integer(1), 
					negate(power(algebraicExprClone(c, arguments[0]), integer(2)))
				),
				rational(-1, 2)
			));
		}

		case ACOS: {
			return multipliedByArgumentDerivative(negate(power(
				sum(integer(1), negate(power(algebraicExprClone(c, arguments[0]), integer(2)))),
				rational(-1, 2)
			)));
		}

		case ATAN: return multipliedByArgumentDerivative(power(
			sum(
				integer(1),
				power(algebraicExprClone(c, arguments[0]), integer(2))
			),
			integer(-1)
		));

		case SYMBOL:
			break;

		}
	}

	if (expr->isFreeOfVariable(variable)) {
		return integer(0);
	}
	
	return std::make_unique<DerivativeExpr>(algebraicExprClone(c, expr), variable);
}
