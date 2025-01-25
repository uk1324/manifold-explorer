#pragma once

#include "Expr.hpp"
#include <optional>
#include <variant>
#include <list>
#include <RefOptional.hpp>

namespace Algebra {

// Maybe change Symbol to VariableSymbol.
std::optional<AlgebraicExprPtr> integrate(const Context& c, const AlgebraicExprPtr& integrand, const Symbol* variableOfIntegration);


}