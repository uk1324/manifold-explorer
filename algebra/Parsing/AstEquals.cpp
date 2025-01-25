#include "AstEquals.hpp"
#include <Assertions.hpp>

using namespace Ast;

bool astEquals(const Expr* a, const Expr* b, bool compareLocations) {
	if (a->type != b->type) {
		return false;
	}

	if (compareLocations && a->sourceLocation != b->sourceLocation) {
		return false;
	}

	switch (a->type) {
		using enum ExprType;
	case CONSTANT: {
		const auto x = static_cast<const ConstantExpr*>(a);
		const auto y = static_cast<const ConstantExpr*>(b);
		return x->numerator == y->numerator && x->denominator == y->denominator;
	}
	case BINARY: {
		const auto x = static_cast<const BinaryExpr*>(a);
		const auto y = static_cast<const BinaryExpr*>(b);
		return x->op == y->op &&
			astEquals(x->lhs, y->lhs, compareLocations) &&
			astEquals(x->rhs, y->rhs, compareLocations);
	}

	case UNARY: {
		const auto x = static_cast<const UnaryExpr*>(a);
		const auto y = static_cast<const UnaryExpr*>(b);
		return x->op == y->op &&
			astEquals(x->operand, y->operand, compareLocations);
	}

	case IDENTIFIER: {
		const auto x = static_cast<const IdentifierExpr*>(a);
		const auto y = static_cast<const IdentifierExpr*>(b);
		return x->identifier == y->identifier;
	}

	case FUNCTION: {
		const auto x = static_cast<const FunctionExpr*>(a);
		const auto y = static_cast<const FunctionExpr*>(b);
		if (x->functionName != y->functionName) {
			return false;
		}
		if (x->arguments.size() != y->arguments.size()) {
			return false;
		}
		for (i32 i = 0; i < x->arguments.size(); i++) {
			if (!astEquals(x->arguments[i], y->arguments[i], compareLocations)) {
				return false;
			}
		}
		return true;
	}

	case DERIVATIVE: {
		const auto x = static_cast<const DerivativeExpr*>(a);
		const auto y = static_cast<const DerivativeExpr*>(b);
		if (x->variableName != y->variableName) {
			return false;
		}
		return astEquals(x->expr, y->expr, compareLocations);
	}

	}

	CHECK_NOT_REACHED();
	return false;
}
