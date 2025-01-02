#include "Expr.hpp"
#include <Assertions.hpp>

using namespace Algebra;

AlgebraicExpr::AlgebraicExpr(AlgebraicExprType type)
	: type(type) {}

bool Algebra::AlgebraicExpr::isUndefined() const {
	return (type == AlgebraicExprType::SYMBOL) && static_cast<const SymbolExpr*>(this)->name == "undefined";
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

IntegerExpr::IntegerExpr(IntegerType value)
	: AlgebraicExpr(AlgebraicExprType::INTEGER) 
	, value(value) {}

RationalExpr::RationalExpr(IntegerType numerator, IntegerType denominator)
	: AlgebraicExpr(AlgebraicExprType::RATIONAL)
	, numerator(numerator)
	, denominator(denominator) {}

SymbolExpr::SymbolExpr(std::string&& name)
	: AlgebraicExpr(AlgebraicExprType::SYMBOL)
	, name(std::move(name)) {}

FunctionExpr::FunctionExpr(std::string&& name, AlgebraicExprList&& arguments)
	: AlgebraicExpr(AlgebraicExprType::FUNCTION)
	, name(std::move(name))
	, arguments(std::move(arguments)) {}

SumExpr::SumExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs)
	: AlgebraicExpr(AlgebraicExprType::SUM) {
	summands.push_back(std::move(lhs));
	summands.push_back(std::move(rhs));
}

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


AlgebraicExprPtr Algebra::PowerExpr::clone() const {
	return std::make_unique<PowerExpr>(algebraicExprClone(base), algebraicExprClone(exponent));
}

AlgebraicExprList algebraicExprListClone(const AlgebraicExprList& list) {
	AlgebraicExprList output;
	for (const auto& expr : list) {
		output.push_back(algebraicExprClone(expr));
	}
	return output;
}

AlgebraicExprPtr Algebra::algebraicExprClone(const AlgebraicExprPtr& exprPtr) {
	const auto expr = exprPtr.get();
	switch (expr->type) {
		using enum AlgebraicExprType;
	case INTEGER: {
		const auto e = static_cast<const IntegerExpr*>(expr);
		return std::make_unique<IntegerExpr>(e->value);
	}
	case RATIONAL: {
		const auto e = static_cast<const RationalExpr*>(expr);
		return std::make_unique<RationalExpr>(e->denominator, e->numerator);
	}
	case SYMBOL: {
		const auto e = static_cast<const SymbolExpr*>(expr);
		return std::make_unique<SymbolExpr>(std::string(e->name));
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		return std::make_unique<FunctionExpr>(std::string(e->name), algebraicExprListClone(e->arguments));
	}
	case SUM: {
		const auto e = static_cast<const SumExpr*>(expr);
		return std::make_unique<SumExpr>(algebraicExprListClone(e->summands));
	}
	case PRODUCT: {
		const auto e = static_cast<const ProductExpr*>(expr);
		return std::make_unique<ProductExpr>(algebraicExprListClone(e->factors));
	}
	case POWER: {
		return expr->asPower()->clone();
	}
	}

	CHECK_NOT_REACHED();
	return std::make_unique<SymbolExpr>("u");
}

AlgebraicExprPtr Algebra::makeUndefined() {
	return std::make_unique<SymbolExpr>("undefined");
}