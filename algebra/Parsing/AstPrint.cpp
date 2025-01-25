#include "AstPrint.hpp"
#include <Put.hpp>

using namespace Ast;

const char* binaryOpName(BinaryOpType op) {
	switch (op) {
		using enum BinaryOpType;
	case  ADD: return "Add";
	case SUBTRACT: return "Sub";
	case MULTIPLY: return "Mul";
	case DIVIDE: return "Div";
	case EXPONENTIATE: return "Pow";
	}
	CHECK_NOT_REACHED();
	return "";
}

const char* unaryOpName(UnaryOpType op) {
	switch (op) {
		using enum UnaryOpType;
	case UnaryOpType::NEGATE:
		return "Neg";
	}
	CHECK_NOT_REACHED();
	return "";
}

void astPrint(const Expr* expr) {
	switch (expr->type) {
		using enum ExprType;

	case CONSTANT: {
		const auto e = static_cast<const ConstantExpr*>(expr);
		putnn("Constant( %, % )", e->numerator, e->denominator);
		return;
	}
	case BINARY: {
		const auto e = static_cast<const BinaryExpr*>(expr);
		putnn("%( ", binaryOpName(e->op));
		astPrint(e->lhs);
		putnn(", ");
		astPrint(e->rhs);
		putnn(" )");
		return;
	}
	case UNARY: {
		const auto e = static_cast<const UnaryExpr*>(expr);
		putnn("%( ", unaryOpName(e->op));
		astPrint(e->operand);
		putnn(" )");
		return;
	}
	case IDENTIFIER: {
		const auto e = static_cast<const IdentifierExpr*>(expr);
		putnn("Identifier( \"%\" )", e->identifier);
		return;
	}
	case FUNCTION: {
		const auto e = static_cast<const FunctionExpr*>(expr);
		putnn("Function( \"%\", ", e->functionName);
		for (i32 i = 0; i < e->arguments.size(); i++) {
			if (i < e->arguments.size() - 1) {
				astPrint(e->arguments[i]);
				putnn(", ");
			}
		}
		putnn(" )");
		return;
	}

	case DERIVATIVE: {
		const auto e = static_cast<const DerivativeExpr*>(expr);
		putnn("D(");
		astPrint(e->expr);
		putnn(", '%')", e->variableName);
		return;
	}
	}

	CHECK_NOT_REACHED();
}
