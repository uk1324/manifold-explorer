#include "Ast.hpp"

using namespace Ast;

ConstantExpr::ConstantExpr(IntType numerator, IntType denominator, i64 start, i64 end)
	: Expr(ExprType::CONSTANT, start, end)
	, numerator(numerator)
	, denominator(denominator) {}

BinaryExpr::BinaryExpr(Expr* lhs, Expr* rhs, BinaryOpType op, i64 start, i64 end)
	: Expr(ExprType::BINARY, start, end)
	, lhs(lhs)
	, rhs(rhs)
	, op(op) {}

UnaryExpr::UnaryExpr(Expr* operand, UnaryOpType op, i64 start, i64 end)
	: Expr(ExprType::UNARY, start, end)
	, op(op)
	, operand(operand) {}

IdentifierExpr::IdentifierExpr(std::string_view identifier, i64 start, i64 end)
	: Expr(ExprType::IDENTIFIER, start, end)
	, identifier(identifier) {}

FunctionExpr::FunctionExpr(std::string_view functionName, std::span<const Expr* const> arguments, i64 start, i64 end)
	: Expr(ExprType::FUNCTION, start, end)
	, functionName(functionName)
	, arguments(arguments) {}

DerivativeExpr::DerivativeExpr(const Expr* expr, std::string_view variableName, i64 start, i64 end)
	: Expr(ExprType::DERIVATIVE, start, end)
	, variableName(variableName)
	, expr(expr) {}

Expr::Expr(ExprType type, i64 start, i64 end)
	: type(type)
	, sourceLocation(SourceLocation::fromStartEnd(start, end)) {}