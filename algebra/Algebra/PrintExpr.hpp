#pragma once

#include "Expr.hpp"

namespace Algebra {

void printAlgebraicExprList(const AlgebraicExprList& list);
void printAlgebraicExprInternal(const AlgebraicExpr* expr);
void printAlgebraicExprNn(const std::unique_ptr<AlgebraicExpr>& expr);
void printAlgebraicExpr(const std::unique_ptr<AlgebraicExpr>& expr);

void printAlgebraicExprListUsingNotation(const AlgebraicExprList& list, std::string_view separator);
void printAlgebraicExprUsingNotation(const AlgebraicExprPtr& exprPtr);
}