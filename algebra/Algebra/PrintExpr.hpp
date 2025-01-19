#pragma once

#include "Expr.hpp"
#include <View.hpp>

namespace Algebra {

void debugPrintAlgebraicExprList(const AlgebraicExprList& list);

void printAlgebraicExprList(const AlgebraicExprList& list);
void printAlgebraicExprInternal(const AlgebraicExpr* expr);
void printAlgebraicExprNn(const std::unique_ptr<AlgebraicExpr>& expr);
void printAlgebraicExpr(const std::unique_ptr<AlgebraicExpr>& expr);
void printAlgebraicExprView(const View<const AlgebraicExprPtr>& view);

void printLogicalExprNn(const LogicalExprPtr& expr);

void printAlgebraicExprListUsingNotation(const AlgebraicExprList& list, std::string_view separator);
void printLogicalExprUsingNotation(const LogicalExprPtr& expr);
void printAlgebraicExprUsingNotation(const AlgebraicExprPtr& exprPtr);
}