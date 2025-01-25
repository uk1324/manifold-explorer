#include "PrintExpr.hpp"
#include <Put.hpp>

using namespace Algebra;

void Algebra::debugPrintAlgebraicExprList(const AlgebraicExprList& list) {
	putnn("[");
	printAlgebraicExprList(list);
	put("]");

}

void Algebra::printAlgebraicExprList(const AlgebraicExprList& list) {
	printAlgebraicExprView(constView(list));
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
		putnn("Symbol( \"%\" )", e->symbol->name);
		return;
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		putnn("Function( \"%\", ", e->function->name);
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
	case DERIVATIVE: {
		const auto e = expr->asDerivative();
		putnn("Derivative( ");
		printAlgebraicExprNn(e->expr);
		putnn(", ");
		putnn("%", e->symbol->name);
		putnn(" )");
	}

	case CONDITIONAL: {
		const auto e = expr->asConditional();
		putnn("Conditional( ");
		for (i32 i = 0; i < e->conditions.size(); i++) {
			printLogicalExprNn(e->conditions[i]);
			putnn(" -> ");
			printAlgebraicExpr(e->results[i]);
			if (i != e->conditions.size()) {
				putnn(", ");
			}
		}
		if (e->conditions.size() != e->results.size()) {
			putnn(", ");
			printAlgebraicExpr(e->results.back());
			putnn(" )");
		} else {
			putnn(" )");
		}
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

void Algebra::printAlgebraicExprView(const View<const AlgebraicExprPtr>& view) {
	for (i32 i = 0; i < view.size(); i++) {
		Algebra::printAlgebraicExprNn(view[i]);
		if (i != view.size() - 1) {
			putnn(", ");
		}
	}
}

void Algebra::printLogicalExprNn(const LogicalExprPtr& expr) {
	switch (expr->type) {
		using enum LogicalExprType;
	case EQUAL: {
		const auto e = expr->asEqual();
		putnn("Eq( ");
		printAlgebraicExprNn(e->lhs);
		putnn(", ");
		printAlgebraicExprNn(e->rhs);
		putnn(" )");
	}
	}

}

void Algebra::printAlgebraicExprListUsingNotation(const AlgebraicExprList& list, std::string_view separator) {
	for (i32 i = 0; i < list.size(); i++) {
		Algebra::printAlgebraicExprUsingNotation(list[i]);
		if (i != list.size() - 1) {
			putnn("%", separator);
		}
	}
}

void Algebra::printLogicalExprUsingNotation(const LogicalExprPtr& expr) {
	switch (expr->type) {
		using enum LogicalExprType;
	case EQUAL: {
		const auto e = expr->asEqual();
		printAlgebraicExprUsingNotation(e->lhs);
		putnn(" = ");
		printAlgebraicExprUsingNotation(e->rhs);
		break;
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
		putnn("%", e->symbol->name);
		return;
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		putnn("%( ", e->function->name);
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

	case DERIVATIVE: {
		const auto e = expr->asDerivative();
		putnn("D( ");
		printAlgebraicExprUsingNotation(e->expr);
		putnn(", % )", e->symbol->name);
		return;
	}

	case CONDITIONAL: {
		const auto e = expr->asConditional();
		putnn("( ");
		for (i32 i = 0; i < e->conditions.size(); i++) {
			printLogicalExprUsingNotation(e->conditions[i]);
			putnn(" -> ");
			printAlgebraicExprUsingNotation(e->results[i]);
			if (i != e->conditions.size()) {
				putnn(", ");
			}
		}
		if (e->conditions.size() != e->results.size()) {
			putnn(", ");
			printAlgebraicExprUsingNotation(e->results.back());
			putnn(" )");
		} else {
			putnn(" )");
		}
	}

	}
	ASSERT_NOT_REACHED();
}