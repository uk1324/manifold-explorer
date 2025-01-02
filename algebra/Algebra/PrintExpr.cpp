#include "PrintExpr.hpp"
#include <Put.hpp>

using namespace Algebra;

void Algebra::printAlgebraicExprList(const AlgebraicExprList& list) {
	for (i32 i = 0; i < list.size(); i++) {
		Algebra::printAlgebraicExprNn(list[i]);
		if (i != list.size() - 1) {
			putnn(", ");
		}
	}
}

void Algebra::printAlgebraicExprInternal(const AlgebraicExpr* expr) {
	switch (expr->type) {
	using enum AlgebraicExprType;
	case INTEGER: {
		const auto e = static_cast<const IntegerExpr*>(expr);
		putnn("Int( % )", e->value);
		return;
	}
			
	case RATIONAL: {
		const auto e = static_cast<const RationalExpr*>(expr);
		putnn("Rational( %, % )", e->numerator, e->denominator);
		return;
	}

	case SYMBOL: {
		const auto e = static_cast<const SymbolExpr*>(expr);
		putnn("Symbol( \"%\" )", e->name);
		return;
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		putnn("Function( \"%\", ", e->name);
		printAlgebraicExprList(e->arguments);
		putnn(" )");
		return;
	}
	case SUM: {
		const auto e = static_cast<const SumExpr*>(expr);
		putnn("Sum( ");
		printAlgebraicExprList(e->summands);
		putnn(" )");
		return;
	}
	case PRODUCT: {
		const auto e = static_cast<const ProductExpr*>(expr);
		putnn("Product( ");
		printAlgebraicExprList(e->factors);
		putnn(" )");
		return;
	}
	case POWER: {
		const auto e = static_cast<const PowerExpr*>(expr);
		putnn("Power( ");
		Algebra::printAlgebraicExprNn(e->base);
		putnn(", ");
		Algebra::printAlgebraicExprNn(e->exponent);
		putnn(" )");
		return;
	}
	}
	ASSERT_NOT_REACHED();
}

void Algebra::printAlgebraicExprNn(const std::unique_ptr<AlgebraicExpr>& expr) {
	Algebra::printAlgebraicExprInternal(expr.get());
}

void Algebra::printAlgebraicExpr(const std::unique_ptr<AlgebraicExpr>& expr) {
	Algebra::printAlgebraicExprNn(expr);
	put("");
}


void Algebra::printAlgebraicExprListUsingNotation(const AlgebraicExprList& list, std::string_view separator) {
	for (i32 i = 0; i < list.size(); i++) {
		Algebra::printAlgebraicExprUsingNotation(list[i]);
		if (i != list.size() - 1) {
			putnn("%", separator);
		}
	}
}

void Algebra::printAlgebraicExprUsingNotation(const AlgebraicExprPtr& exprPtr) {
	const auto expr = exprPtr.get();
	switch (expr->type) {
	using enum AlgebraicExprType;
	case INTEGER: {
		const auto e = static_cast<const IntegerExpr*>(expr);
		putnn("%", e->value);
		return;
	}
			
	case RATIONAL: {
		const auto e = static_cast<const RationalExpr*>(expr);
		putnn("(%/%)", e->numerator, e->denominator);
		return;
	}

	case SYMBOL: {
		const auto e = static_cast<const SymbolExpr*>(expr);
		putnn("%", e->name);
		return;
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		putnn("%( ", e->name);
		printAlgebraicExprListUsingNotation(e->arguments, ", ");
		putnn(" )");
		return;
	}
	case SUM: {
		const auto e = static_cast<const SumExpr*>(expr);
		putnn("( ");
		printAlgebraicExprListUsingNotation(e->summands, " + ");
		putnn(" )");
		return;
	}
	case PRODUCT: {
		const auto e = static_cast<const ProductExpr*>(expr);
		putnn("( ");
		printAlgebraicExprListUsingNotation(e->factors, " * ");
		putnn(" )");
		return;
	}
	case POWER: {
		const auto e = static_cast<const PowerExpr*>(expr);
		putnn("( ");
		Algebra::printAlgebraicExprUsingNotation(e->base);
		putnn("^");
		Algebra::printAlgebraicExprUsingNotation(e->exponent);
		putnn(" )");
		return;
	}
	}
	ASSERT_NOT_REACHED();
}