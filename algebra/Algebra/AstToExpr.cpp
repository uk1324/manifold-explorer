#include "AstToExpr.hpp"
#include <Assertions.hpp>

std::unique_ptr<Algebra::AlgebraicExpr> astToExpr(const Ast::Expr* expr) {
	auto defaultValue = []() -> std::unique_ptr<Algebra::AlgebraicExpr> {
		return std::make_unique<Algebra::IntegerExpr>(1);
	};

	switch (expr->type) {
		using enum Ast::ExprType;
	case CONSTANT: {
		const auto e = static_cast<const Ast::ConstantExpr*>(expr);
		if (e->denominator == 1) {
			return std::make_unique<Algebra::IntegerExpr>(e->numerator);
		} else {
			return std::make_unique<Algebra::RationalExpr>(e->numerator, e->denominator);
		}
	}

	case BINARY: {
		const auto e = static_cast<const Ast::BinaryExpr*>(expr);
		switch (e->op) {
			using enum Ast::BinaryOpType;
			case ADD: return std::make_unique<Algebra::SumExpr>(
				astToExpr(e->lhs), 
				astToExpr(e->rhs)
			);
			case SUBTRACT: return std::make_unique<Algebra::SumExpr>(
				astToExpr(e->lhs),
				std::make_unique<Algebra::ProductExpr>(
					std::make_unique<Algebra::IntegerExpr>(-1),
					astToExpr(e->rhs) 
				)
			);
			case MULTIPLY: return std::make_unique<Algebra::ProductExpr>(
				astToExpr(e->lhs), 
				astToExpr(e->rhs)
			);
			case DIVIDE: return std::make_unique<Algebra::ProductExpr>(
				astToExpr(e->lhs),
				std::make_unique<Algebra::PowerExpr>(
					astToExpr(e->rhs),
					std::make_unique<Algebra::IntegerExpr>(-1)
				)
			);
			case EXPONENTIATE: return std::make_unique<Algebra::PowerExpr>(
				astToExpr(e->lhs), 
				astToExpr(e->rhs)
			);
			return defaultValue();
		}
	}
	case UNARY: {
		const auto e = static_cast<const Ast::UnaryExpr*>(expr);
		switch (e->op) {
			using enum Ast::UnaryOpType;
		case NEGATE:
			return std::make_unique<Algebra::ProductExpr>(
				std::make_unique<Algebra::IntegerExpr>(-1),
				astToExpr(e->operand) 
			);
		}
		return defaultValue();
	}
	case IDENTIFIER: {
		const auto e = static_cast<const Ast::IdentifierExpr*>(expr);
		return std::make_unique<Algebra::SymbolExpr>(std::string(e->identifier));
	}
	case FUNCTION:
		const auto e = static_cast<const Ast::FunctionExpr*>(expr);
		Algebra::AlgebraicExprList arguments;
		for (const auto& argument : e->arguments) {
			arguments.push_back(astToExpr(argument));
		}
		return std::make_unique<Algebra::FunctionExpr>(std::string(e->functionName), std::move(arguments));
	}
	return defaultValue();
}
