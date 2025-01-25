#pragma once

#include "Expr.hpp"
#include <optional>
#include "Functions.hpp"

namespace Algebra {

// d/dt (x(t) = f(x(t)) * g(t)
std::optional<EqualExpr> separableTrySolve(const Context& c, const AlgebraicExprPtr& f, const AlgebraicExprPtr& g, const VariableSymbol* dependent, const VariableSymbol* independent);

// a dx/dt + b = 0
std::optional<EqualExpr> tryExact(const Context& c, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* dependent, const VariableSymbol* independent);
std::optional<EqualExpr> exactTrySolve(const Context& c, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* dependent, const VariableSymbol* independent);

// Try finding integrating factor m = m(w) for a dx + bt = 0.
std::optional<AlgebraicExprPtr> tryIntegratingFactor(const Context& c, const AlgebraicExprPtr& w, AlgebraicExprPtr&& bXMinusAt, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b, const VariableSymbol* x, const VariableSymbol* t);

// Solves equation = 0.
std::optional<LogicalExprPtr> odeSolve(const Context& c, const AlgebraicExprPtr& equation, const FunctionSymbol* dependent, const VariableSymbol* independent);

}