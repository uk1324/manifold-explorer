#pragma once

#include "token.hpp"
#include "sourceInfo.hpp"
#include <span>

// Using Expr* pointers instead of references to prevent bugs like writing auto x = someExpr() and then passing it into some struct. Which would leave a dangling reference. To fix this you would need to write auto&.

namespace Ast {

enum class ExprType {
	CONSTANT,
	BINARY,
	UNARY,
	IDENTIFIER,
	FUNCTION,
	DERIVATIVE,
};

struct Expr {
	Expr(ExprType type, i64 start, i64 end);
	const ExprType type;
	SourceLocation sourceLocation;
};

//using FloatType = f64;
using IntType = i64;

struct ConstantExpr : public Expr {
	ConstantExpr(IntType numerator, IntType denominator, i64 start, i64 end);
	
	IntType numerator;
	IntType denominator;
};

enum class BinaryOpType {
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	EXPONENTIATE
};

struct BinaryExpr : public Expr {
	BinaryExpr(Expr* lhs, Expr* rhs, BinaryOpType op, i64 start, i64 end);
	Expr* lhs;
	Expr* rhs;
	BinaryOpType op;
};

enum class UnaryOpType {
	NEGATE,
};

struct UnaryExpr : public Expr {
	UnaryExpr(Expr* operand, UnaryOpType op, i64 start, i64 end);

	Expr* operand;
	UnaryOpType op;
};

struct IdentifierExpr : public Expr {
	IdentifierExpr(std::string_view identifier, i64 start, i64 end);

	std::string_view identifier;
};

struct FunctionExpr : public Expr {
	FunctionExpr(std::string_view functionName, std::span<const Expr* const> arguments, i64 start, i64 end);

	std::string_view functionName;
	std::span<const Expr* const> arguments;
};

struct DerivativeExpr : public Expr {
	DerivativeExpr(const Expr* expr, std::string_view variableName, i64 start, i64 end);

	const Expr* expr;
	std::string_view variableName;
};

}