#include "Context.hpp"

using namespace Algebra;

Algebra::Context::Context()
	: sin(Sin())
	, cos(Cos())
	, ln(Ln())
	, abs(Abs())
	, tan(Tan())
	, asin(Asin())
	, acos(Acos())
	, atan(Atan())
	, undefined(UndefinedSymbol()) {

	functions.push_back(&sin);
	functions.push_back(&cos);
	functions.push_back(&ln);
	functions.push_back(&abs);
	functions.push_back(&tan);
	functions.push_back(&asin);
	functions.push_back(&acos);
	functions.push_back(&atan);

	symbols.push_back(&undefined);
	symbols.push_back(&e);
}

VariableSymbol* Context::addVariable(std::string&& variableName) {
	variables.push_back(VariableSymbol(std::move(variableName)));
	auto ptr = &variables.back();
	symbols.push_back(ptr);
	return ptr;
}

AlgebraicExprPtr Algebra::Context::makeUndefined() const {
	return std::make_unique<SymbolExpr>(&undefined);
}
