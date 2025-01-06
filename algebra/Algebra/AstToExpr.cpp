#include "AstToExpr.hpp"
#include <Assertions.hpp>
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
			return defaultValue();
		}
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
		for (const auto& s : c.symbols) {
			if (s->name == e->identifier) {
				return symbol(s);
			}
		}
		return std::unexpected(AstToExprErrorUndefinedSymbol{ .symbolName = std::string(e->identifier) });
	}
	case FUNCTION:
		const auto e = static_cast<const Ast::FunctionExpr*>(expr);
		Algebra::AlgebraicExprList arguments;
		for (const auto& argument : e->arguments) {
			auto a = astToExpr(c, argument);
			TEST(a);
			arguments.push_back(std::move(*a));
		}
		for (const auto& f : c.functions) {
			if (f->name == e->functionName) {
				return function(f, std::move(arguments));
			}
		}
		return std::unexpected(AstToExprErrorUndefinedSymbol{ .symbolName = std::string(e->functionName) });
	}
	return defaultValue();
}
