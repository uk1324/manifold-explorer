#pragma once

#include "Expr.hpp"

namespace Algebra {

AlgebraicExprList basicSimplifiyList(const Context& c, const AlgebraicExprList& list);
AlgebraicExprPtr basicSimplifiy(const Context& c, const AlgebraicExprPtr& expr);
LogicalExprPtr basicSimplifiyLogical(const Context& c, const LogicalExprPtr& expr);

bool algebraicExprEquals(const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr);
bool anyAlgebraicExprEquals(const AlgebraicExprPtr& expr, View<const AlgebraicExprPtr> exprs);
bool anyAlgebraicExprEquals(const AlgebraicExprPtr& expr, const AlgebraicExprList& exprs);
bool logicalExprEquals(const LogicalExprPtr& aExpr, const LogicalExprPtr& bExpr);
// The expressions have to be in simplified form.
bool algebraicExprLessThan(const Context& c, const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr);
bool logicalExprLessThan(const Context& c, const LogicalExprPtr& aExpr, const LogicalExprPtr& bExpr);

bool isSimplifiedExpr(const Context& c, const AlgebraicExprPtr& expr);
bool isSimplifiedExpr(const Context& c, const LogicalExprPtr& expr);

}