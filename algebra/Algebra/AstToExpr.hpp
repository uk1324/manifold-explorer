#pragma once

#include "../Parsing/Ast.hpp"
#include "Context.hpp"
#include "Expr.hpp"
#include <expected>
#include <variant>

struct AstToExprErrorUndefinedSymbol {
	std::string symbolName;
};

using AstToExprError = std::variant<AstToExprErrorUndefinedSymbol>;

void putAstToExprError(std::ostream& os, const AstToExprError& error);

std::expected<Algebra::AlgebraicExprPtr, AstToExprError> astToExpr(const Algebra::Context& c, const Ast::Expr* expr);