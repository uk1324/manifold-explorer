#pragma once

#include "Functions.hpp"
#include <list>

namespace Algebra {

struct Context {
	Context();

	std::list<VariableSymbol> variables;
	Symbol* addVariable(std::string&& variableName);

	const Sin sin;
	const Cos cos;
	const Ln ln;

	const EulersNumberSymbol e;

	std::vector<const Function*> functions;

	const UndefinedSymbol undefined;

	std::vector<const Symbol*> symbols;

	AlgebraicExprPtr makeUndefined() const;
};

}