#include "AstToExpr.hpp"
#include <Assertions.hpp>
#include <Put.hpp>
#include <Overloaded.hpp>
#include "ConstructionHelpers.hpp"
#include <optional>

#define TEST(expectedExpr) \
	if (!expectedExpr.has_value()) { \
		return expectedExpr; \
	}

#define CONVERT_BINARY_OP(operation) \
	{ \
		auto a = astToExpr(c, e->lhs); \
		TEST(a); \
		auto b = astToExpr(c, e->rhs); \
		TEST(b); \
		return operation(std::move(*a), std::move(*b)); \
	}

void putAstToExprError(std::ostream& os, const AstToExprError& error) {
	std::visit(overloaded{
		[&](const AstToExprErrorUndefinedSymbol& e) {
			put(os, "Undefined symbol %.", e.symbolName);
		},
		[&](const AstToExprErrorWrongArity& e) {
			put(os, "'%' expects % arguments, but found %.", e.functionName, e.correctArity, e.gotArity);
		}
	}, error);
}


std::expected<const Algebra::Symbol*, AstToExprError> tryFindSymbol(const Algebra::Context& c, std::string_view name) {
	for (const auto& s : c.symbols) {
		if (s->name == name) {
			return s;
		}
	}
	return std::unexpected(AstToExprErrorUndefinedSymbol{ .symbolName = std::string(name) });
}

std::expected<Algebra::AlgebraicExprPtr, AstToExprError> astToExpr(const Algebra::Context& c, const Ast::Expr* expr) {
	auto defaultValue = []() -> std::unique_ptr<Algebra::AlgebraicExpr> {
		return std::make_unique<Algebra::IntegerExpr>(1);
	};
	using namespace AlgebraConstuctionHelpers;

	switch (expr->type) {
		using enum Ast::ExprType;
	case CONSTANT: {
		const auto e = static_cast<const Ast::ConstantExpr*>(expr);
		if (e->denominator == 1) {
			return integer(e->numerator);
		} else {
			return rational(e->numerator, e->denominator);
		}
	}

	case BINARY: {
		const auto e = static_cast<const Ast::BinaryExpr*>(expr);
		switch (e->op) {
			using enum Ast::BinaryOpType;
			case ADD: CONVERT_BINARY_OP(sum)
			case SUBTRACT: CONVERT_BINARY_OP(difference)
			case MULTIPLY: CONVERT_BINARY_OP(product)
			case DIVIDE: CONVERT_BINARY_OP(division)
			case EXPONENTIATE: CONVERT_BINARY_OP(power)
		}
		ASSERT_NOT_REACHED();
		return defaultValue();
	}
	case UNARY: {
		const auto e = static_cast<const Ast::UnaryExpr*>(expr);
		switch (e->op) {
			using enum Ast::UnaryOpType;
		case NEGATE: {
			auto o = astToExpr(c, e->operand);
			TEST(o);
			return negate(std::move(*o));
		}
		}
		return defaultValue();
	}
	case IDENTIFIER: {
		const auto e = static_cast<const Ast::IdentifierExpr*>(expr);
		auto s = tryFindSymbol(c, e->identifier);
		if (!s.has_value()) {
			return std::unexpected(std::move(s.error()));
		}
		return symbol(*s);
	}
	case FUNCTION: {
		const auto e = static_cast<const Ast::FunctionExpr*>(expr);
		Algebra::AlgebraicExprList arguments;
		for (const auto& argument : e->arguments) {
			auto a = astToExpr(c, argument);
			TEST(a);
			arguments.push_back(std::move(*a));
		}
		for (const auto& f : c.functions) {
			if (f->name != e->functionName) {
				continue;
			}
			if (f->arity != arguments.size()) {
				return std::unexpected(AstToExprErrorWrongArity{
					.correctArity = f->arity,
					.gotArity = i32(arguments.size()),
					.functionName = f->name,
				});
			}
			return function(f, std::move(arguments));
		}
		const auto sqrtName = "sqrt";
		const auto sqrtArity = 1;
		if (e->functionName == sqrtName) {
			if (arguments.size() != sqrtArity) {
				return std::unexpected(AstToExprErrorWrongArity{
					.correctArity = sqrtArity,
					.gotArity = i32(arguments.size()),
					.functionName = sqrtName,
				});
			}
			return power(std::move(arguments[0]), rational(1, 2));
		}
		return std::unexpected(AstToExprErrorUndefinedSymbol{ .symbolName = std::string(e->functionName) });
	}
	case DERIVATIVE: {
		const auto e = static_cast<const Ast::DerivativeExpr*>(expr);
		auto s = tryFindSymbol(c, e->variableName);
		if (!s.has_value()) {
			return std::unexpected(std::move(s.error()));
		}
		auto expr = astToExpr(c, e->expr);
		TEST(expr);
		return derivative(std::move(*expr), s.value());
	}
	}
	return defaultValue();
}
