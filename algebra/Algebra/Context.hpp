#pragma once

#include "Functions.hpp"

namespace Algebra {

struct Context {
	Context();

	const Sin sin;
	const Cos cos;
	const Ln ln;

	std::vector<const Function*> functions;

	const UndefinedSymbol undefined;

	std::vector<const Symbol*> symbols;

	AlgebraicExprPtr makeUndefined() const;
};

}