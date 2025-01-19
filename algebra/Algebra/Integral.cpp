#include "Integral.hpp"
#include "ConstructionHelpers.hpp"
#include "Derivative.hpp"
#include <Assertions.hpp>
#include "Context.hpp"
#include "Simplification.hpp"

using namespace Algebra;
using namespace AlgebraConstuctionHelpers;

#define INTEGRAL_TABLE_STEP(antiderivative) \
	{ \
		auto a = antiderivative; \
		if (integrationSteps.has_value()) { \
			integrationSteps->steps.push_back(IntegralTableStep{ \
				algebraicExprClone(c, integrand), \
				algebraicExprClone(c, a) \
			}); \
		} \
		return a; \
	}

bool isNotIntegerValue(const AlgebraicExprPtr& expr, IntegerType value) {
	if (expr->isInteger()) {
		return expr->asInteger()->value != value;
	} else if (expr->isRational()) {
		// Assuming expr is simplified.
		return true;
	} else if (expr->isSymbol()) {
		const auto s = expr->asSymbol()->symbol;
		if (s->type == SymbolType::E || s->type == SymbolType::PI) {
			return true;
		}
	}
	// Could check for squares, square roots and the like.
	return false;
};

AlgebraicExprPtr powerRule(const Context& c, const AlgebraicExprPtr& integrand, const AlgebraicExprPtr& b, const AlgebraicExprPtr& e, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps) {
	// TODO: Change to ln(abs(x))
	auto case1 = [&]() { return function(c.ln, algebraicExprClone(c, b)); };
	if (e->isIntegerValue(-1)) {
		INTEGRAL_TABLE_STEP(case1());
	}

	auto case2 = [&]() {
		return division(
			power(algebraicExprClone(c, b), sum(algebraicExprClone(c, e), integer(1))),
			sum(algebraicExprClone(c, e), integer(1))
		);
	};
	if (isNotIntegerValue(e, -1)) {
		INTEGRAL_TABLE_STEP(case2());
	}

	INTEGRAL_TABLE_STEP(conditional(
		equals(algebraicExprClone(c, b), integer(-1)), case1(),
		case2()
	));
}

// b^x
AlgebraicExprPtr exponential(const Context& c, const AlgebraicExprPtr& integrand, const AlgebraicExprPtr& base, const Symbol* exponent, std::optional<IntegrationStepsContext&> integrationSteps) {
	auto case1 = [&]() {
		return integer(0);
	};

	if (base->isIntegerValue(1)) {
		INTEGRAL_TABLE_STEP(case1());
	}

	auto case2 = [&]() {
		return division(
			algebraicExprClone(c, integrand), 
			function(c.ln, algebraicExprClone(c, base))
		);
	};
	if (isNotIntegerValue(base, 1)) {
		return case2();
	}

	return conditional(
		equals(algebraicExprClone(c, base), integer(1)), case1(),
		case2()
	);
}

std::optional<AlgebraicExprPtr> integralTable(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps) {

	if (integrand->isFreeOfVariable(variableOfIntegration)) {
		return product(algebraicExprClone(c, integrand), symbol(variableOfIntegration));
	}

	if (integrand->isSymbolValue(variableOfIntegration)) {
		return powerRule(c, integrand, integrand, integer(1), variableOfIntegration, integrationSteps);
	} else if (integrand->isPower()) {
		const auto i = integrand->asPower();
		const auto& b = i->base;
		const auto& e = i->exponent;
	
		if (b->isSymbolValue(variableOfIntegration) && e->isFreeOfVariable(variableOfIntegration)) {
			return powerRule(c, integrand, b, e, variableOfIntegration, integrationSteps);
		}

		if (b->isFreeOfVariable(variableOfIntegration) && e->isSymbolValue(variableOfIntegration)) {
			return exponential(c, integrand, b, variableOfIntegration, integrationSteps);
		}

		// 1/(1 + x^2)
		if (e->isIntegerValue(-1) && e->isSum()) {
			const auto eSum = e->asSum();
			if (eSum->summands.size() == 2 && eSum->summands[0]->isIntegerValue(1) && eSum->summands[1]->isPower()) {
				const auto eSumPow = eSum->summands[1]->asPower();
				if (eSumPow->base->isSymbolValue(variableOfIntegration) && eSumPow->exponent->isIntegerValue(2)) {
					return function(c.atan, symbol(variableOfIntegration));
				}
			}
		}

	} else if (integrand->isFunction()) {
		const auto i = integrand->asFunction();
		if (i->arguments.size() == 1 && i->arguments[0]->isSymbolValue(variableOfIntegration)) {
			auto arg = [&]() {
				return algebraicExprClone(c, i->arguments[0]);
			};

			if (i->function == &c.sin) {
				INTEGRAL_TABLE_STEP(negate(function(&c.cos, arg())));
			} else if (i->function == &c.cos) {
				INTEGRAL_TABLE_STEP(function(&c.sin, arg()));
			}
		}
	}

	return std::nullopt;
}

std::optional<AlgebraicExprPtr> integralLinearity(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps) {

	auto decompose = [&c, &variableOfIntegration](const AlgebraicExprPtr& expr) {
		if (!expr->isProduct()) {
			return IntegralLinearityStep::Factorization{
				std::nullopt, algebraicExprClone(c, expr)
			};
		}
		const auto e = expr->asProduct();
		AlgebraicExprList constant;
		AlgebraicExprList nonConstant;
		for (const auto& factor : e->factors) {
			if (factor->isFreeOfVariable(variableOfIntegration)) {
				constant.push_back(algebraicExprClone(c, factor));
			} else {
				nonConstant.push_back(algebraicExprClone(c, factor));
			}
		}
		if (constant.size() <= 0) {
			return IntegralLinearityStep::Factorization{
				std::nullopt, algebraicExprClone(c, expr)
			};
		}
		if (nonConstant.size() <= 0) {
			// This is what mathdf does. For example if you input a + x it splits it into a * int 1 + int x.
			// Cases like 1 + a are already handled by the integral table (the free of variable of integration case).
			nonConstant.push_back(integer(1));
		}
		return IntegralLinearityStep::Factorization{
			tryProduct(std::move(constant)), tryProduct(std::move(nonConstant))
		};
	};

	std::vector<IntegralLinearityStep::Factorization> summands;

	if (integrand->isProduct()) {
		auto decomposition = decompose(integrand);
		if (!decomposition.constant.has_value()) {
			return std::nullopt;
		}
		summands.push_back(std::move(decomposition));
	} else if (integrand->isSum()) {
		for (const auto& summand : integrand->asSum()->summands) {
			summands.push_back(decompose(summand));
		}
	} else {
		return std::nullopt;
	}

	AlgebraicExprList result;
	std::vector<IntegralLinearityStep::Summand> stepSummands;

	for (auto& summand : summands) {
		std::optional<AlgebraicExprPtr> antiderivative;
		if (integrationSteps.has_value()) {
			IntegrationStepsContext steps{ .substitutionSymbols = integrationSteps->substitutionSymbols };
			antiderivative = integrate(c, summand.nonConstant, variableOfIntegration, steps);
			stepSummands.push_back(IntegralLinearityStep::Summand{
				.factorization = {
					summand.constant.has_value()
						? std::optional(algebraicExprClone(c, *summand.constant))
						: std::nullopt,
					algebraicExprClone(c, summand.nonConstant)
				},
				.nonConstantIntegral = algebraicExprClone(c, *antiderivative),
				.steps = std::move(steps.steps)
			});
			
		} else {
			antiderivative = integrate(c, summand.nonConstant, variableOfIntegration, integrationSteps);
		}

		if (!antiderivative.has_value()) {
			return std::nullopt;
		}

		if (summand.constant.has_value()) {
			result.push_back(product(std::move(*summand.constant), std::move(*antiderivative)));
		} else {
			result.push_back(std::move(*antiderivative));
		}
	}
	return trySum(std::move(result));
}

void addUnique(std::vector<AlgebraicExprPtr>& v, AlgebraicExprPtr&& expr) {
	for (const auto& value : v) {
		if (algebraicExprEquals(value, expr)) {
			return;
		}
	}
	v.push_back(std::move(expr));
}

void findSubstitutions(const Context& c, const AlgebraicExprPtr& expr, const Symbol* variableOfIntegration, std::vector<AlgebraicExprPtr>& result) {
	// The commented out code bellow would probably make a lot of duplicate calls, because of cases like for example the root expression is not free of the variable, but some subexpression is. Then when you call isFreeOfVariable it returns false after having checked all the branches, but then you recursively call it on the branched again. It is probably better to check just before adding a candidate.
	/*if (expr->isFreeOfVariable(variableOfIntegration)) {
		
	}*/

	auto maybeAdd = [&](const AlgebraicExprPtr& expr) -> bool {
		if (expr->isSymbolValue(variableOfIntegration)) {
			return false;
		}
		if (expr->isFreeOfVariable(variableOfIntegration)) {
			return true;
		}
		addUnique(result, algebraicExprClone(c, expr));
		return false;
	};

	auto listFindSubstitutions = [&](const AlgebraicExprList& l) {
		for (const auto& e : l) {
			findSubstitutions(c, e, variableOfIntegration, result);
		}
	};

	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
	case SYMBOL:
		break;
	case FUNCTION: {
		const auto e = expr->asFunction();
		if (maybeAdd(expr)) {
			break;
		}
		for (const auto& argument : e->arguments) {
			if (maybeAdd(argument)) {
				continue;
			}
			findSubstitutions(c, argument, variableOfIntegration, result);
		}
		break;
	}

	case SUM: {
		const auto e = expr->asSum();
		listFindSubstitutions(e->summands);
		break;
	}
	case PRODUCT: 
		listFindSubstitutions(expr->asProduct()->factors);
		break;

	case POWER: {
		const auto e = expr->asPower();
		maybeAdd(e->base);
		maybeAdd(e->exponent);
		break;
	}
	case DERIVATIVE: {
		break;
	}
	case CONDITIONAL: {
		break;
	}
	}
}

AlgebraicExprPtr structuralyIdenticalSubstitiute(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& toReplace, const AlgebraicExprPtr& replacement);

AlgebraicExprList structuralyIdenticalSubstitiuteList(const Context& c, const AlgebraicExprList& exprList, const AlgebraicExprPtr& toReplace, const AlgebraicExprPtr& replacement) {
	AlgebraicExprList result;
	for (const auto& expr : exprList) {
		result.push_back(structuralyIdenticalSubstitiute(c, expr, toReplace, replacement));
	}
	return result;
}

AlgebraicExprPtr structuralyIdenticalSubstitiute(const Context& c, const AlgebraicExprPtr& expr, const AlgebraicExprPtr& toReplace, const AlgebraicExprPtr& replacement) {

	if (algebraicExprEquals(expr, toReplace)) {
		return algebraicExprClone(c, replacement);
	}

	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
	case SYMBOL:
	case DERIVATIVE: // TODO:
	case CONDITIONAL:
		return algebraicExprClone(c, expr);

	case FUNCTION: {
		const auto e = expr->asFunction();
		return function(e->function, structuralyIdenticalSubstitiuteList(c, e->arguments, toReplace, replacement));
	}

	case SUM:
		return sum(structuralyIdenticalSubstitiuteList(c, expr->asSum()->summands, toReplace, replacement));

	case PRODUCT:
		return product(structuralyIdenticalSubstitiuteList(c, expr->asProduct()->factors, toReplace, replacement));

	case POWER: {
		const auto e = expr->asPower();
		return power(
			structuralyIdenticalSubstitiute(c, e->base, toReplace, replacement),
			structuralyIdenticalSubstitiute(c, e->exponent, toReplace, replacement)
		);
	}

	}

	ASSERT_NOT_REACHED();
	return c.makeUndefined();
}

std::optional<AlgebraicExprPtr> integralSubstitutions(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps) {
	AlgebraicExprList substitutionsToTry;
	findSubstitutions(c, integrand, variableOfIntegration, substitutionsToTry);

	for (const auto& substitution : substitutionsToTry) {
		auto symbolData = VariableSymbol("u");
		VariableSymbol* symbolPtr;
		if (integrationSteps.has_value()) {
			integrationSteps->substitutionSymbols.push_back(std::move(symbolData));
			symbolPtr = &integrationSteps->substitutionSymbols.back();
		} else {
			symbolPtr = &symbolData;
		}

		const auto substitiuted = structuralyIdenticalSubstitiute(c, integrand, substitution, symbol(symbolPtr));
		const auto substitutionResult = basicSimplifiy(c,
			division(
				algebraicExprClone(c, substitiuted),
				derivative(c, substitution, variableOfIntegration)
			)
		);
		if (!substitutionResult->isFreeOfVariable(variableOfIntegration)) {
			continue;
		}

		std::optional<AlgebraicExprPtr> result;
		if (integrationSteps.has_value()) {
			IntegrationStepsContext steps{ .substitutionSymbols = integrationSteps->substitutionSymbols };
			result = integrate(c, substitutionResult, variableOfIntegration, steps);
			if (result.has_value()) {
				integrationSteps->steps.push_back(IntegralSubstitutionStep{
					.substitution = algebraicExprClone(c, substitution),
					.substitutionVariable = symbolPtr,
					.steps = std::move(steps.steps),
					.result = algebraicExprClone(c, *result),
				});
			}
		} else {
			result = integrate(c, substitutionResult, symbolPtr, std::nullopt);
		}
		if (result.has_value()) {
			return structuralyIdenticalSubstitiute(c, *result, symbol(symbolPtr), substitution);
		}
	}
	return std::nullopt;
}

std::optional<AlgebraicExprPtr> Algebra::integrate(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps) {

	auto result = integralTable(c, integrand, variableOfIntegration, integrationSteps);
	if (result.has_value()) {
		return basicSimplifiy(c, *result);
	}

	result = integralLinearity(c, integrand, variableOfIntegration, integrationSteps);
	if (result.has_value()) {
		return basicSimplifiy(c, *result);
	}

	result = integralSubstitutions(c, integrand, variableOfIntegration, integrationSteps);
	if (result.has_value()) {
		return basicSimplifiy(c, *result);
	}
	// TODO: Expand the expression then try.
	return std::nullopt;
}

Algebra::IntegrationStep::IntegrationStep(IntegrationStep&& other)
	: type(type) {
	switch (type) {
		using enum IntegrationStepType;
	case TABLE:
		new (&table) IntegralTableStep(std::move(other.table));
		break;

	case LINEARITY:
		new (&linearity) IntegralLinearityStep(std::move(other.linearity));
		break;

	case SUBSTITUTION:
		new (&substitution) IntegralLinearityStep(std::move(other.linearity));
		break;
	}

	ASSERT_NOT_REACHED();
}

IntegrationStep::IntegrationStep(IntegralTableStep&& s)
	: table(std::move(s))
	, type(IntegrationStepType::TABLE) {}

IntegrationStep::IntegrationStep(IntegralLinearityStep&& s)
	: linearity(std::move(s))
	, type(IntegrationStepType::LINEARITY) {}

IntegrationStep::IntegrationStep(IntegralSubstitutionStep&& s)
	: substitution(std::move(s))
	, type(IntegrationStepType::SUBSTITUTION) {}

Algebra::IntegrationStep::~IntegrationStep() {
	switch (type) {
		using enum IntegrationStepType;

	case TABLE: table.~IntegralTableStep(); break;
	case LINEARITY: linearity.~IntegralLinearityStep(); break;
	case SUBSTITUTION: substitution.~IntegralSubstitutionStep(); break;
	}
	ASSERT_NOT_REACHED();
}
