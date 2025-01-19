#include "Expr.hpp"
#include <Assertions.hpp>
#include "Context.hpp"

using namespace Algebra;

AlgebraicExpr::AlgebraicExpr(AlgebraicExprType type)
	: type(type) {}

bool Algebra::AlgebraicExpr::isUndefined() const {
	return type == AlgebraicExprType::SYMBOL &&
		asSymbol()->symbol->type == SymbolType::UNDEFINED;
}

bool Algebra::AlgebraicExpr::isIntegerValue(IntegerType value) const {
	return type == AlgebraicExprType::INTEGER && static_cast<const IntegerExpr*>(this)->value == value;
}

bool Algebra::AlgebraicExpr::isInteger() const {
	return type == AlgebraicExprType::INTEGER;
}

bool Algebra::AlgebraicExpr::isRational() const {
	return type == AlgebraicExprType::RATIONAL;
}

bool Algebra::AlgebraicExpr::isPower() const {
	return type == AlgebraicExprType::POWER;
}

bool Algebra::AlgebraicExpr::isProduct() const {
	return type == AlgebraicExprType::PRODUCT;
}

bool Algebra::AlgebraicExpr::isSum() const {
	return type == AlgebraicExprType::SUM;
}

bool Algebra::AlgebraicExpr::isSymbol() const {
	return type == AlgebraicExprType::SYMBOL;
}

bool Algebra::AlgebraicExpr::isSymbolValue(const Symbol* symbol) const {
	return isSymbol() && asSymbol()->symbol == symbol;
}

bool Algebra::AlgebraicExpr::isFunction() const {
	return type == AlgebraicExprType::FUNCTION;
}

bool Algebra::AlgebraicExpr::isDerivative() const {
	return type == AlgebraicExprType::DERIVATIVE;
}

bool Algebra::AlgebraicExpr::isConditional() const {
	return type == AlgebraicExprType::CONDITIONAL;
}

const IntegerExpr* Algebra::AlgebraicExpr::asInteger() const {
	ASSERT(isInteger());
	return static_cast<const IntegerExpr*>(this);
}

const RationalExpr* Algebra::AlgebraicExpr::asRational() const {
	ASSERT(isRational());
	return static_cast<const RationalExpr*>(this);
}

const PowerExpr* Algebra::AlgebraicExpr::asPower() const {
	return const_cast<AlgebraicExpr*>(this)->asPower();
}

PowerExpr* Algebra::AlgebraicExpr::asPower() {
	ASSERT(isPower());
	return static_cast<PowerExpr*>(this);
}

const ProductExpr* Algebra::AlgebraicExpr::asProduct() const {
	return const_cast<AlgebraicExpr*>(this)->asProduct();
}

ProductExpr* Algebra::AlgebraicExpr::asProduct() {
	ASSERT(isProduct());
	return static_cast<ProductExpr*>(this);
}

const SumExpr* Algebra::AlgebraicExpr::asSum() const {
	return const_cast<AlgebraicExpr*>(this)->asSum();
}

SumExpr* Algebra::AlgebraicExpr::asSum() {
	ASSERT(isSum());
	return static_cast<SumExpr*>(this);
}

const SymbolExpr* Algebra::AlgebraicExpr::asSymbol() const {
	return const_cast<AlgebraicExpr*>(this)->asSymbol();
}

SymbolExpr* Algebra::AlgebraicExpr::asSymbol() {
	ASSERT(isSymbol());
	return static_cast<SymbolExpr*>(this);
}

const FunctionExpr* Algebra::AlgebraicExpr::asFunction() const {
	return const_cast<AlgebraicExpr*>(this)->asFunction();
}

FunctionExpr* Algebra::AlgebraicExpr::asFunction() {
	ASSERT(isFunction());
	return static_cast<FunctionExpr*>(this);
}

const DerivativeExpr* Algebra::AlgebraicExpr::asDerivative() const {
	return const_cast<AlgebraicExpr*>(this)->asDerivative();
}

DerivativeExpr* Algebra::AlgebraicExpr::asDerivative() {
	ASSERT(isDerivative());
	return static_cast<DerivativeExpr*>(this);
}

const ConditionalExpr* Algebra::AlgebraicExpr::asConditional() const {
	return const_cast<AlgebraicExpr*>(this)->asConditional();
}

ConditionalExpr* Algebra::AlgebraicExpr::asConditional() {
	ASSERT(isConditional());
	return static_cast<ConditionalExpr*>(this);
}

bool algebraicExprListFreeOfVariable(const AlgebraicExprList& list, const Symbol* symbol) {
	for (const auto& a : list) {
		if (!a->isFreeOfVariable(symbol)) {
			return false;
		}
	}
	return true;
}

bool AlgebraicExpr::isFreeOfVariable(const Symbol* variable) const {
	switch (type) {
		using enum AlgebraicExprType;
	case INTEGER:
	case RATIONAL:
		return true;

	case SYMBOL: {
		const auto e = asSymbol();
		return e->symbol != variable;
	}

	case FUNCTION: {
		const auto e = asFunction();
		return algebraicExprListFreeOfVariable(e->arguments, variable);
	}
	case SUM: {
		const auto e = asSum();
		return algebraicExprListFreeOfVariable(e->summands, variable);
	}
	case PRODUCT: {
		const auto e = asProduct();
		return algebraicExprListFreeOfVariable(e->factors, variable);
	}
	case POWER: {
		const auto e = asPower();
		return e->base->isFreeOfVariable(variable) && e->exponent->isFreeOfVariable(variable);
	}

	case DERIVATIVE: {
		const auto e = asDerivative();
		// The symbol doesn't have to be free of the variable for example Derivative(1, x) is free of x.
		return e->expr->isFreeOfVariable(variable);
	}

	case CONDITIONAL: {
		const auto e = asConditional();
	}

	}
	return false;
}

LogicalExpr::LogicalExpr(LogicalExprType type) 
	: type(type) {}

bool Algebra::LogicalExpr::isEqual() const {
	return type == LogicalExprType::EQUAL;
}

EqualExpr* Algebra::LogicalExpr::asEqual() {
	ASSERT(isEqual());
	return static_cast<EqualExpr*>(this);
}

const EqualExpr* Algebra::LogicalExpr::asEqual() const {
	return const_cast<LogicalExpr*>(this)->asEqual();
}

Function::Function(FunctionType type, std::string&& name, i32 arity) 
	: type(type)
	, name(std::move(name))
	, arity(arity) {}

Symbol::Symbol(SymbolType type, std::string&& name)
	: type(type)
	, name(std::move(name)) {}

UndefinedSymbol::UndefinedSymbol()
	: Symbol(SymbolType::UNDEFINED, "undefined") {}

VariableSymbol::VariableSymbol(std::string&& name) 
	: Symbol(SymbolType::VARIABLE, std::move(name)) {}

EulersNumberSymbol::EulersNumberSymbol() 
	: Symbol(SymbolType::E, "e") {}

IntegerExpr::IntegerExpr(IntegerType value)
	: AlgebraicExpr(AlgebraicExprType::INTEGER) 
	, value(value) {}

RationalExpr::RationalExpr(IntegerType numerator, IntegerType denominator)
	: AlgebraicExpr(AlgebraicExprType::RATIONAL)
	, numerator(numerator)
	, denominator(denominator) {}

SymbolExpr::SymbolExpr(const Symbol* symbol)
	: AlgebraicExpr(AlgebraicExprType::SYMBOL)
	, symbol(symbol) {}

FunctionExpr::FunctionExpr(const Function* function, AlgebraicExprList&& arguments)
	: AlgebraicExpr(AlgebraicExprType::FUNCTION)
	, function(function)
	, arguments(std::move(arguments)) {}

SumExpr::SumExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs)
	: AlgebraicExpr(AlgebraicExprType::SUM) {
	summands.push_back(std::move(lhs));
	summands.push_back(std::move(rhs));
}

ConditionalExpr::ConditionalExpr(LogicalExprList&& conditions, AlgebraicExprList&& results) 
	: AlgebraicExpr(AlgebraicExprType::CONDITIONAL)
	, conditions(std::move(conditions))
	, results(std::move(results)) {}

SumExpr::SumExpr(AlgebraicExprList&& summands)
	: AlgebraicExpr(AlgebraicExprType::SUM)
	, summands(std::move(summands)) {}

ProductExpr::ProductExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs) 
	: AlgebraicExpr(AlgebraicExprType::PRODUCT) {
	factors.push_back(std::move(lhs));
	factors.push_back(std::move(rhs));
}

ProductExpr::ProductExpr(AlgebraicExprList&& factors)
	: AlgebraicExpr(AlgebraicExprType::PRODUCT)
	, factors(std::move(factors)) {}

PowerExpr::PowerExpr(AlgebraicExprPtr&& base, AlgebraicExprPtr&& exponent)
	: AlgebraicExpr(AlgebraicExprType::POWER)
	, base(std::move(base))
	, exponent(std::move(exponent)) {}


AlgebraicExprPtr Algebra::PowerExpr::clone(const Context& c) const {
	return std::make_unique<PowerExpr>(algebraicExprClone(c, base), algebraicExprClone(c, exponent));
}

DerivativeExpr::DerivativeExpr(AlgebraicExprPtr&& expr, const Symbol* symbol) 
	: AlgebraicExpr(AlgebraicExprType::DERIVATIVE)
	, expr(std::move(expr))
	, symbol(symbol) {}

AlgebraicExprList Algebra::algebraicExprListClone(const Context& c, const AlgebraicExprList& list) {
	AlgebraicExprList output;
	for (const auto& expr : list) {
		output.push_back(algebraicExprClone(c, expr));
	}
	return output;
}

AlgebraicExprPtr Algebra::algebraicExprClone(const Context& c, const AlgebraicExprPtr& exprPtr) {

	const auto expr = exprPtr.get();
	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER: {
		const auto e = static_cast<const IntegerExpr*>(expr);
		return std::make_unique<IntegerExpr>(e->value);
	}
	case RATIONAL: {
		const auto e = static_cast<const RationalExpr*>(expr);
		return std::make_unique<RationalExpr>(e->numerator, e->denominator);
	}
	case SYMBOL: {
		const auto e = static_cast<const SymbolExpr*>(expr);
		return std::make_unique<SymbolExpr>(e->symbol);
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		return std::make_unique<FunctionExpr>(e->function, algebraicExprListClone(c, e->arguments));
	}
	case SUM: {
		const auto e = static_cast<const SumExpr*>(expr);
		return std::make_unique<SumExpr>(algebraicExprListClone(c, e->summands));
	}
	case PRODUCT: {
		const auto e = static_cast<const ProductExpr*>(expr);
		return std::make_unique<ProductExpr>(algebraicExprListClone(c, e->factors));
	}
	case POWER: {
		return expr->asPower()->clone(c);
	}
	case DERIVATIVE: {
		const auto e = expr->asDerivative();
		return std::make_unique<DerivativeExpr>(algebraicExprClone(c, e->expr), e->symbol);
	}

	case CONDITIONAL: {
		const auto e = expr->asConditional();
		return std::make_unique<ConditionalExpr>(logicalExprListClone(c, e->conditions), algebraicExprListClone(c, e->results));
	}
	}

	CHECK_NOT_REACHED();
	return c.makeUndefined();
}

LogicalExprPtr Algebra::logicalExprClone(const Context& c, const LogicalExprPtr& expr) {

	switch (expr->type) {
		using enum LogicalExprType;
	case EQUAL: {
		const auto e = expr->asEqual();
		return std::make_unique<EqualExpr>(algebraicExprClone(c, e->lhs), algebraicExprClone(c, e->rhs));
	}
	}
	ASSERT_NOT_REACHED();
}

LogicalExprList Algebra::logicalExprListClone(const Context& c, const LogicalExprList& list) {
	LogicalExprList result;
	for (const auto& e : list) {
		result.push_back(logicalExprClone(c, e));
	}
	return result;
}

EqualExpr::EqualExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs) 
	: LogicalExpr(LogicalExprType::EQUAL) 
	, lhs(std::move(lhs))
	, rhs(std::move(rhs)) {}