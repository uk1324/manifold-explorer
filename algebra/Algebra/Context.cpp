#include "Context.hpp"

using namespace Algebra;

Algebra::Context::Context()
	: sin(Sin())
	, cos(Cos())
	, ln(Ln())
	, undefined(UndefinedSymbol()) {

	functions.push_back(&sin);
	functions.push_back(&cos);
	functions.push_back(&ln);

	symbols.push_back(&undefined);
	symbols.push_back(&e);
}

Symbol* Context::addVariable(std::string&& variableName) {
	variables.push_back(VariableSymbol(std::move(variableName)));
	auto ptr = &variables.back();
	symbols.push_back(ptr);
	return ptr;
}

AlgebraicExprPtr Algebra::Context::makeUndefined() const {
	return std::make_unique<SymbolExpr>(&undefined);
}
