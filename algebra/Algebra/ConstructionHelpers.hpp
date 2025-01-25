#pragma once

#include "Expr.hpp"

namespace AlgebraConstuctionHelpers {

using namespace Algebra;

AlgebraicExprPtr sum(AlgebraicExprList&& list);
AlgebraicExprPtr sum(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b);
AlgebraicExprPtr sum(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b, AlgebraicExprPtr&& c);
AlgebraicExprPtr trySum(AlgebraicExprList&& summands);
AlgebraicExprPtr difference(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b);
AlgebraicExprPtr negate(AlgebraicExprPtr&& e);
AlgebraicExprPtr product(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b);
AlgebraicExprPtr product(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b, AlgebraicExprPtr&& c);
AlgebraicExprPtr product(AlgebraicExprList&& factors);
AlgebraicExprPtr tryProduct(AlgebraicExprList&& factors);
AlgebraicExprPtr division(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b);
AlgebraicExprPtr inverse(AlgebraicExprPtr&& a);

template <typename ...T>
AlgebraicExprPtr product(T&&... args) {
	AlgebraicExprList list;
	((list.push_back(std::move(args))), ...);
	return std::make_unique<ProductExpr>(std::move(list));
}

AlgebraicExprPtr symbol(const Symbol* symbol);
AlgebraicExprPtr symbol(const Symbol& symbol);
AlgebraicExprPtr integer(IntegerType value);
AlgebraicExprPtr rational(IntegerType numerator, IntegerType denominator);
AlgebraicExprPtr power(AlgebraicExprPtr&& base, AlgebraicExprPtr&& exponent);
AlgebraicExprPtr function(const Function* function, AlgebraicExprPtr&& argument);
AlgebraicExprPtr function(const Function* function, AlgebraicExprList&& arguments);
AlgebraicExprPtr function(const Function& function, AlgebraicExprPtr&& argument);
AlgebraicExprPtr function(const Function& function, AlgebraicExprList&& arguments);
AlgebraicExprPtr derivative(AlgebraicExprPtr&& expr, const Symbol* symbol);

AlgebraicExprPtr conditional(LogicalExprList&& condition, AlgebraicExprList&& results);
AlgebraicExprPtr conditional(
	LogicalExprPtr&& c0, AlgebraicExprPtr&& r0,
	AlgebraicExprPtr&& r1);

LogicalExprPtr equals(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs);

}