#pragma once

#include "Expr.hpp"

namespace Algebra {

// Should simplification be called during or after differentiation.
AlgebraicExprPtr derivativeUnsimplified(Context& c, const AlgebraicExprPtr& expr, const Symbol* variable);
AlgebraicExprPtr derivative(Context& c, const AlgebraicExprPtr& expr, const Symbol* variable);

}