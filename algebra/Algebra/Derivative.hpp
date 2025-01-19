#pragma once

#include "Expr.hpp"

namespace Algebra {

// Should simplification be called during or after differentiation.
AlgebraicExprPtr derivativeUnsimplified(const Context& c, const AlgebraicExprPtr& expr, const Symbol* variable);
AlgebraicExprPtr derivative(const Context& c, const AlgebraicExprPtr& expr, const Symbol* variable);

}