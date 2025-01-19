#pragma once

#include "../Parsing/Ast.hpp"
#include "Context.hpp"
#include "Expr.hpp"
#include <expected>
#include <variant>

struct AstToExprErrorUndefinedSymbol {
	std::string symbolName;
};

struct AstToExprErrorWrongArity {
	i32 correctArity;
	i32 gotArity;
	std::string functionName;
};

using AstToExprError = std::variant<
	AstToExprErrorUndefinedSymbol,
	AstToExprErrorWrongArity
>;

void putAstToExprError(std::ostream& os, const AstToExprError& error);

std::expected<Algebra::AlgebraicExprPtr, AstToExprError> astToExpr(const Algebra::Context& c, const Ast::Expr* expr);