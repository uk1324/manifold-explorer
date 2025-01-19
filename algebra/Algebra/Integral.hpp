#pragma once

#include "Expr.hpp"
#include <optional>
#include <variant>
#include <list>
#include <RefOptional.hpp>

namespace Algebra {

struct IntegrationStep;
using IntegrationSteps = std::vector<IntegrationStep>;

struct IntegralTableStep {
	AlgebraicExprPtr input;
	AlgebraicExprPtr output;
};

struct IntegralLinearityStep {
	struct Factorization {
		std::optional<AlgebraicExprPtr> constant;
		AlgebraicExprPtr nonConstant;
	};
	struct Summand {
		Factorization factorization;
		AlgebraicExprPtr nonConstantIntegral;
		IntegrationSteps steps;
	};
	std::vector<Summand> summands;
};

struct IntegralSubstitutionStep {
	AlgebraicExprPtr substitution;
	VariableSymbol* substitutionVariable;
	IntegrationSteps steps;
	AlgebraicExprPtr result;
};

/*
I don't think using std::variant is possible in this case without introducing pointless allocations by storing a unique ptr to a thing instead of the thing itself or doing other weird things.
I want to have vectors of IntegrationStep inside for example IntegralLinearityStep, but because there is no way to forward declare a using declaration I can't access this type. One way to work around it would be to introduce a wrapper type for IntegrationStep so then you can forward declare it.
google std::variant recursive
//using IntegrationStep = std::variant<IntegralTableStep, IntegralLinearityStep, IntegralSubstitutionStep>;
*/

enum class IntegrationStepType {
	TABLE,
	LINEARITY,
	SUBSTITUTION,
};

struct IntegrationStep {
	union {
		IntegralTableStep table;
		IntegralLinearityStep linearity;
		IntegralSubstitutionStep substitution;
	};

	IntegrationStepType type;
	IntegrationStep(IntegrationStep&& other);
	IntegrationStep(IntegralTableStep&& s);
	IntegrationStep(IntegralLinearityStep&& s);
	IntegrationStep(IntegralSubstitutionStep&& s);
	~IntegrationStep();
};



struct IntegrationStepsContext {
	std::vector<IntegrationStep> steps;
	std::list<VariableSymbol>& substitutionSymbols;
};
// Maybe change Symbol to VariableSymbol.
std::optional<AlgebraicExprPtr> integrate(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration, std::optional<IntegrationStepsContext&> integrationSteps);


}