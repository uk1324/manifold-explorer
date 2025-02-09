#include "OdeSolve.hpp"
#include "Polynomial.hpp"
#include "Context.hpp"
#include "Simplification.hpp"
#include "Integral.hpp"
#include "Derivative.hpp"
#include "ConstructionHelpers.hpp"
using namespace AlgebraConstuctionHelpers;

using namespace Algebra;

struct BernoulliOrLinearEquationForm {
	// a(t) x + b(t) x^r, where r is a rational number different than 1.
	AlgebraicExprPtr a;
	AlgebraicExprPtr b;
	AlgebraicExprPtr exponent;
	// If b = 0 we have a homogenous linear differentation equation
	// If the previous doesn't hold and r = 0 we have a inhomogenous linear differentation equation
};

std::optional<BernoulliOrLinearEquationForm> tryMatchBernoulliOrLinearEquation(const Context& c, const AlgebraicExprPtr& expr, const VariableSymbol* x, const VariableSymbol* t) {
	
	AlgebraicExprList summands;
	if (expr->isSum()) {
		summands = algebraicExprListClone(c, expr->asSum()->summands);
	} else {
		summands.push_back(algebraicExprClone(c, expr));
	}

	AlgebraicExprList aSum;
	AlgebraicExprList bSum;

	std::optional<AlgebraicExprPtr> r;

	enum FactorType {
		X, X_TO_R, CONST, ERROR
	};
	auto findFactorType = [&r, &x, &c](const AlgebraicExprPtr& factor) -> FactorType {
		if (factor->isSymbolValue(x)) {
			return X;
		} else if (factor->isPower()) {
			const auto p = factor->asPower();
			const auto& exponent = p->exponent;
			if (p->base->isSymbolValue(x) && (exponent->isInteger() || exponent->isRational())) {
				if (r.has_value()) {
					if (!algebraicExprEquals(exponent, factor)) {
						return ERROR;
					}
				} else {
					r = algebraicExprClone(c, exponent);
				}
				return X_TO_R;
			}
		}
		if (factor->isFreeOfVariable(x)) {
			return CONST;
		} 
		return ERROR;
	};

	for (auto& summand : summands) {
		AlgebraicExprList factors;
		
		if (summand->isProduct()) {
			factors = std::move(summand->asProduct()->factors);
		} else {
			factors.push_back(std::move(summand));
		}

		std::optional<FactorType> factorType;
		AlgebraicExprList constantFactors;

		for (auto& factor : factors) {
			const auto result = findFactorType(factor);
			if (result == ERROR) {
				return std::nullopt;
			}
			if (factorType == CONST) {
				constantFactors.push_back(std::move(factor));
			} else {
				if (factorType.has_value()) {
					if (result != factorType) {
						// Cases like x * x^r
						return std::nullopt;
					}
				} else {
					factorType = result;
				}
			} 
		}

		if (factorType.has_value()) {
			if (*factorType == X) {
				aSum.push_back(tryProduct(std::move(constantFactors)));
			} else { // X_TO_R
				bSum.push_back(tryProduct(std::move(constantFactors)));
			}
		} else {
			// Constant factor in inhomegenous linear equation a(t)x + b(t)x^0.
			bSum.push_back(tryProduct(std::move(constantFactors)));
			r = integer(0);
		}
	}

	if (!r.has_value()) {
		return std::nullopt;
	}

	return BernoulliOrLinearEquationForm{
		.a = trySum(std::move(aSum)),
		.b = trySum(std::move(bSum)),
		.exponent = std::move(*r)
	};

	/*
	Alternatively something like this could be done

	r = findXWithPowerDifferentThan1(expr);

	if (!r.hasValue()) {
		// handle linear equation.
	}

	if (polynomialDegree(expr, { x, x^r }) != 1) {
		return null;
	}

	// I don't think it would be possible to get the constant coefficient using just coefficient (non polynomial version with a single variable argument), because then it would do things like this I think.
	// coefficient(x + x^(1/2), x, 0) = x^(1/2)

	if (constantCoefficient(expr, { x, x^r } != 0) {
		return std::nullopt;
	}

	const auto a = coefficient(expr, x);
	if (!isFreeOf(a, x) {
		return null;
	}
	const auto b = coefficient(expr, x);
	if (!isFreeOf(b, x^r) {
		return null;
	}
	return { a, b, r }
	*/

	// In sympy the is the match function
	// a = Wildcard('a', exclude = [x])
	// b = Wildcard('b', exclude = [x])
	// c = Wildcard('c', exclude = [x])
	// expr.match(a * x + b * x**c)

	// Google "pattern matching symbolic expressions"
	// https://www.numbas.org.uk/behind-the-design/pattern-matching.html#previous-work

	// Matching against many patterns at once can also be optimized by expoiting the similarities between the patterns.
	// https://www.google.com/search?client=firefox-b-d&q=pattern+matchin+with+asciotativty+and+commutativity
	// https://stackoverflow.com/questions/8335187/pattern-matching-with-associative-and-commutative-operators
};

bool isDerivative(const AlgebraicExprPtr& e, const FunctionSymbol* dependent, const VariableSymbol* independent) {
	if (!e->isDerivative()) {
		return false;
	}
	const auto d = e->asDerivative();
	if (d->symbol != independent) {
		return false;
	}
	if (!d->expr->isFunction()) {
		return false;
	}
	const auto f = d->expr->asFunction();
	return f->function == dependent;
}

// d/dt (x(t)) A + B
// Where A and B are free of derivatives of x.
struct OdeForm1 {
	AlgebraicExprPtr a;
	AlgebraicExprPtr b;
};

i32 derivativeOrder(const AlgebraicExprPtr& expr, const FunctionSymbol* dependent, const VariableSymbol* independent);

i32 derivativeListOrder(const AlgebraicExprList& list, const FunctionSymbol* dependent, const VariableSymbol* independent) {

	i32 order = -1;
	for (const auto& e : list) {
		order = std::max(order, derivativeOrder(e, dependent, independent));
	}
	return order;
}

// Dependent should have arity 1.
i32 derivativeOrder(const AlgebraicExprPtr& expr, const FunctionSymbol* dependent, const VariableSymbol* independent) {
	switch (expr->type) {
		using enum AlgebraicExprType;
		case INTEGER:
		case RATIONAL:
		case SYMBOL:
			return -1;

		case FUNCTION:
			return derivativeListOrder(expr->asFunction()->arguments, dependent, independent);
		case SUM:
			return derivativeListOrder(expr->asSum()->summands, dependent, independent);
		case PRODUCT:
			return derivativeListOrder(expr->asProduct()->factors, dependent, independent);
		case POWER: {
			const auto e = expr->asPower();
			return std::max(
				derivativeOrder(e->base, dependent, independent),
				derivativeOrder(e->exponent, dependent, independent));
		}
			 
		case DERIVATIVE: {
			const auto e = expr->asDerivative();
			if (isDerivative(expr, dependent, independent)) {
				return 1;
			} else {
				return derivativeOrder(e->expr, dependent, independent);
			}
		}

		case CONDITIONAL: {
			const auto e = expr->asConditional();
			return derivativeListOrder(e->results, dependent, independent);
		}
	}
}

// e1 x' + e0 x + c0 is really just a first order polynomial in { x', x }
// In the case of a differential equation we also need to make sure that the coefficients are free of derivatives.
//struct LinearCombinationWithConstant {
//	// The last coefficient is the constant.
//	std::vector<AlgebraicExpr> coefficients;
//};
//
//std::optional<LinearCombinationWithConstant> tryMatchLinearCombinationWithConstant() {
//
// 
// If you want to match an expression of the form a x'' + b x' + c x + d
// You may implement just an algorithm that matches this kind of expression, but you might also realise that it's a linear combination in { x'', x', x } with a constant added, but then you might realize that it's just a first order general multivariable polynomial in { x'', x', x }, but then you might go even further and have it be polynopmial like exprsesion with non natural number expressions. When to stop.
// I general the problem is that a computational definition must be given and I am not sure if it should be given in terms of more general primitives of made from scratch.
//}

std::optional<OdeForm1> tryMatchOdeForm1(const Context& c, const AlgebraicExprPtr& equation, const FunctionSymbol* dependent, const VariableSymbol* independent) {

	const auto variable = derivative(function(dependent, symbol(independent)), independent);
	const auto variables = constView(variable);
	if (!isGeneralPolynomial(equation, variables)) {
		return std::nullopt;
	}

	// What about 0 dx/dt + b
	if (generalPolynomialDegree(equation, variables) != 1) {
		return std::nullopt;
	}

	auto a = generalPolynomialCoefficient(c, equation, variable, 1);
	auto b = generalPolynomialCoefficient(c, equation, variable, 0);
	if (!a.has_value() || !b.has_value()) {
		return std::nullopt;
	}

	if (derivativeOrder(*a, dependent, independent) > 0 || derivativeOrder(*b, dependent, independent) > 0) {
		return std::nullopt;
	}

	return OdeForm1{ std::move(*a), std::move(*b) };
}

struct SeparateVariablesResult {
	AlgebraicExprPtr a;
	AlgebraicExprPtr b;
};

std::optional<SeparateVariablesResult> trySeparateVariables(const Context& c, const AlgebraicExprPtr& expr, const VariableSymbol* a, const VariableSymbol* b) {
	// This probably should first use some algorithm to factor the expression and then take the needed factors.

	AlgebraicExprList factors;
	if (expr->isProduct()) {
		const auto e = expr->asProduct();
		factors = algebraicExprListClone(c, e->factors);
	} else {
		factors.push_back(algebraicExprClone(c, expr));
	}

	AlgebraicExprList factorsThanDependOnA;
	AlgebraicExprList factorsThanDependOnB;
	for (const auto& f : factors) {
		if (f->isFreeOfVariable(a)) {
			factorsThanDependOnB.push_back(algebraicExprClone(c, f));
		} else if (f->isFreeOfVariable(b)) {
			factorsThanDependOnA.push_back(algebraicExprClone(c, f));
		} else {
			return std::nullopt;
		}
	}
	return SeparateVariablesResult{
		tryProduct(std::move(factorsThanDependOnA)),
		tryProduct(std::move(factorsThanDependOnB))
	};
}

std::optional<EqualExpr> Algebra::separableTrySolve(const Context& c, const AlgebraicExprPtr& f, const AlgebraicExprPtr& g, const VariableSymbol* dependent, const VariableSymbol* independent) {
	
	// x' = f(x)g(t)
	// 1/f(x) dx = g(t) dt
	auto fIntegral = integrate(c, basicSimplifiy(c, inverse(algebraicExprClone(c, f))), dependent);
	if (!fIntegral.has_value()) {
		return std::nullopt;
	}
	auto gIntegral = integrate(c, g, independent);
	if (!gIntegral.has_value()) {
		return std::nullopt;
	}
	return EqualExpr(std::move(*fIntegral), std::move(*gIntegral));
}

std::optional<EqualExpr> Algebra::tryExact(const Context& c, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* dependent, const VariableSymbol* independent) {
	// a dx/dt + b = 0
	// a dx + b dt = 0
	const auto& x = dependent;
	const auto& t = independent;

	auto aT = derivative(c, a, t);
	auto bX = derivative(c, b, x);
	auto bXMinusAt = basicSimplifiy(c, difference(algebraicExprClone(c, aT), algebraicExprClone(c, bX)));
	
	if (bXMinusAt->isIntegerValue(0)) {
		return exactTrySolve(c, a, b, dependent, independent);
	}
	
	// Lazily evaluated array.
	auto integratingFactorW = [&](i32 i) -> AlgebraicExprPtr {
		switch (i) {
		case 0: return symbol(dependent);
		case 1: return symbol(independent);
			// TODO: Instead of substituting w = xt could substitiute x = w/t. This might simplify better.
		case 2: return product(symbol(dependent), symbol(independent));
		}
	};
	for (i32 i = 0; i < 2; i++) {
		const auto w = integratingFactorW(i);
		const auto integratingFactor = tryIntegratingFactor(c, w, algebraicExprClone(c, bXMinusAt), a, b, dependent, independent);
		if (!integratingFactor.has_value()) {
			continue;
		}
		auto solution = exactTrySolve(
			c,
			product(algebraicExprClone(c, a), algebraicExprClone(c, *integratingFactor)),
			product(algebraicExprClone(c, b), algebraicExprClone(c, *integratingFactor)),
			dependent,
			independent
		);
		if (solution.has_value()) {
			return solution;
		}
	}

	return std::nullopt;
}

std::optional<EqualExpr> Algebra::exactTrySolve(const Context& c, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* dependent, const VariableSymbol* independent) {
	
	// We have the equation a dx + b dt = 0. We want to find U such that U_x = a and U_t = b.
	// So we have a system of partial differential equations.
	// Assuming that U in C_2 it is sufficient and nescessary that a_t = b_t, because then we have U_xt = U_tx.
	// Here we already assume that a_t = b_t.
	// U_x(x, t) = a(x, t) => U(x, t) = I(a, x) = e(x, t) + d(t)
	// Partially differentating we get
	// U_t = e_t(x, t) + d'(t)
	// e_t(x, t) + d'(t) = b
	// d'(t) = b - e_t(x, t)
	// d(t) = Int(b - e_t(x, t), t)

	auto e = integrate(c, a, dependent);
	if (!e.has_value()) {
		return std::nullopt;
	}
	auto e_t = derivative(c, *e, independent);
	const auto dPrime = basicSimplifiy(c, difference(algebraicExprClone(c, b), std::move(e_t)));
	auto d = integrate(c, dPrime, independent);
	if (!d.has_value()) {
		return std::nullopt;
	}
	return EqualExpr(sum(std::move(*e), std::move(*d)), integer(0));
}

std::optional<AlgebraicExprPtr> Algebra::tryIntegratingFactor(const Context& c, const AlgebraicExprPtr& w, AlgebraicExprPtr&& bXMinusAt, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* x, const VariableSymbol* t) {
	
	/*
	Integrating factor
	Let w = w(x, t).
	If the equation is complete then
	m(w) a dx + m(w) b dt = 0
	Let P = m(w) a and Q = m(w) b
	Then
	P_t = m' w_t a + m a_t
	Q_x = m' w_x b + m b_x
	and P_t = Q_x so that
	m' w_t a + m a_t = m' w_x b + m b_x
	m' w_t a - m' w_x b = m b_x - m a_t
	m' (w_t a - w_x b) = m (b_x - m a_t)
	m'/m = (b_x - a_t) / (w_t a - w_x b)

	If the rhs only depends on w
	This equation is separable and we can solve it for m.
	It also turns out that multiplting by m is sufficient for making the equation complete.

	Solving we get
	ln|m(w)| = I((b_x - a_t) / (w_t a - w_x b), w) + c
	m(w) = c e^(I((b_x - a_t) / (w_t a - w_x b), w))
	We can choose any nonzero c so it's probably simplest to choose 1.
	*/
	auto wX = derivative(c, w, x);
	auto wT = derivative(c, w, t);
	
	auto mPrimeOverM = division(
		std::move(bXMinusAt), 
		difference(
			product(std::move(wT), algebraicExprClone(c, a)),
			product(std::move(wX), algebraicExprClone(c, b))
		)
	);
	// TODO: Use rational simplify
	mPrimeOverM = basicSimplifiy(c, std::move(mPrimeOverM));
	VariableSymbol wSymbol("");
	const auto wSymbolExpr = symbol(&wSymbol);
	const auto substituited = structuralyIdenticalSubstitiute(c, mPrimeOverM, w, wSymbolExpr);
	if (substituited->isFreeOfVariable(x) && substituited->isFreeOfVariable(t)) {
		return std::nullopt;
	}
	auto i = integrate(c, substituited, &wSymbol);
	if (!i.has_value()) {
		return std::nullopt;
	}
	auto integratingFactor = power(symbol(&c.e), std::move(*i));
	return structuralyIdenticalSubstitiute(c, integratingFactor, wSymbolExpr, w);
}

std::optional<LogicalExprPtr> Algebra::odeSolve(const Context& c, const AlgebraicExprPtr& equation, const FunctionSymbol* dependent, const VariableSymbol* independent) {
	
	const auto x = function(dependent, symbol(independent));
	const auto t = symbol(independent);

	if (dependent->arity != 1) {
		return std::nullopt;
	}

	// TODO: Maybe check that this is a differential equation. 
	// Have to handle cases like derivatives that depend on x(t) and derivatives that are taken with respect to a different variable. Actually derivatives with respect to another variable will be zero, because the arity should be 1. Also things like delay differential equations need to be handled.

	// Simplify to some canonical form then try to match agains patterns.
	// Could first try doing without more simplification then try with more simplification. For example exercise 2 on page 167 gives an example.

 	const auto form1 = tryMatchOdeForm1(c, equation, dependent, independent);
	if (form1.has_value()) {
		const auto xOfT = function(dependent, symbol(independent));
		VariableSymbol x("");
		const auto xExpr = symbol(&x);

		auto aAlgebraic = structuralyIdenticalSubstitiute(c, form1->a, xOfT, xExpr);
		auto bAlgebraic = structuralyIdenticalSubstitiute(c, form1->b, xOfT, xExpr);
		// a x' + b = 0
		// x' = -b/a
		const auto xPrime = basicSimplifiy(c, negate(division(
			algebraicExprClone(c, bAlgebraic),
			algebraicExprClone(c, aAlgebraic)
		)));
		const auto separated = trySeparateVariables(c, xPrime, &x, independent);

		auto processSolution = [&](const EqualExpr& expr) -> LogicalExprPtr {
			return equals(
				structuralyIdenticalSubstitiute(c, expr.lhs, xExpr, xOfT),
				structuralyIdenticalSubstitiute(c, expr.rhs, xExpr, xOfT)
			);
		};

		if (separated.has_value()) {
			const auto solution = separableTrySolve(c, separated->a, separated->b, &x, independent);
			if (solution.has_value()) {
				return processSolution(*solution);
			}
		} 
		
		{
			const auto solution = tryExact(c, aAlgebraic, bAlgebraic, &x, independent);
			if (solution.has_value()) {
				return processSolution(*solution);
			}
		}
	}

	return std::nullopt;

	// Maybe try to make non implicit
	//return structuralyIdenticalSubstitiute(solution;
}
