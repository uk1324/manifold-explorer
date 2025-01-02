#pragma once

#include "Expr.hpp"
#include "../Parsing/Ast.hpp"

std::unique_ptr<Algebra::AlgebraicExpr> astToExpr(const Ast::Expr* expr);