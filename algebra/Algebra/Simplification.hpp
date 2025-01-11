#pragma once

#include "Expr.hpp"

namespace Algebra {

AlgebraicExprPtr basicSimplifiy(const Context& c, const AlgebraicExprPtr& expr);

bool algebraicExprEquals(const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr);
// The expressions have to be in simplified form.
bool algebraicExprLessThan(const Context& c, const AlgebraicExprPtr& aExprPtr, const AlgebraicExprPtr& bExprPtr);

bool isSimplifiedExpr(const Context& c, const AlgebraicExprPtr& expr);

}