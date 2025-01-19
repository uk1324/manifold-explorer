#include "ConstructionHelpers.hpp"
#include "Functions.hpp"
#include <Assertions.hpp>

using namespace AlgebraConstuctionHelpers;

AlgebraicExprPtr AlgebraConstuctionHelpers::sum(AlgebraicExprList&& list) {
	return std::make_unique<SumExpr>(std::move(list));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::sum(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b) {
	return std::make_unique<SumExpr>(std::move(a), std::move(b));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::sum(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b, AlgebraicExprPtr&& c) {
	AlgebraicExprList list;
	list.push_back(std::move(a));
	list.push_back(std::move(b));
	list.push_back(std::move(c));
	return std::make_unique<SumExpr>(std::move(list));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::trySum(AlgebraicExprList&& summands) {
	ASSERT(summands.size() >= 1);
	if (summands.size() == 1) {
		return std::move(summands[0]);
	}
	return sum(std::move(summands));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::difference(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b) {
	return sum(
		std::move(a),
		negate(std::move(b))
	);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::negate(AlgebraicExprPtr&& e) {
	return product(integer(-1), std::move(e));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::product(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b) {
	return std::make_unique<ProductExpr>(std::move(a), std::move(b));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::product(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b, AlgebraicExprPtr&& c) {
	AlgebraicExprList list;
	list.push_back(std::move(a));
	list.push_back(std::move(b));
	list.push_back(std::move(c));
	return std::make_unique<ProductExpr>(std::move(list));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::product(AlgebraicExprList&& factors) {
	return std::make_unique<ProductExpr>(std::move(factors));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::tryProduct(AlgebraicExprList&& factors) {
	ASSERT(factors.size() >= 1);
	if (factors.size() == 1) {
		return std::move(factors[0]);
	}
	return product(std::move(factors));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::division(AlgebraicExprPtr&& a, AlgebraicExprPtr&& b) {
	return product(std::move(a), inverse(std::move(b)));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::inverse(AlgebraicExprPtr&& a) {
	return power(std::move(a), integer(-1));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::symbol(const Symbol* symbol) {
	return std::make_unique<SymbolExpr>(symbol);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::symbol(const Symbol& symbol) {
	return ::symbol(&symbol);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::integer(IntegerType value) {
	return std::make_unique<IntegerExpr>(value);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::rational(IntegerType numerator, IntegerType denominator) {
	return std::make_unique<RationalExpr>(numerator, denominator);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::power(AlgebraicExprPtr&& base, AlgebraicExprPtr&& exponent) {
	return std::make_unique<PowerExpr>(std::move(base), std::move(exponent));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::function(const Function* function, AlgebraicExprPtr&& argument) {
	AlgebraicExprList arguments;
	arguments.push_back(std::move(argument));
	return std::make_unique<FunctionExpr>(
		std::move(function),
		std::move(arguments)
	);
}

AlgebraicExprPtr AlgebraConstuctionHelpers::function(const Function* function, AlgebraicExprList&& arguments) {
	return std::make_unique<FunctionExpr>(std::move(function), std::move(arguments));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::function(const Function& function, AlgebraicExprPtr&& argument) {
	return ::function(&function, std::move(argument));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::function(const Function& function, AlgebraicExprList&& arguments) {
	return ::function(&function, std::move(arguments));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::conditional(LogicalExprList&& condition, AlgebraicExprList&& results) {
	return std::make_unique<ConditionalExpr>(std::move(condition), std::move(results));
}

AlgebraicExprPtr AlgebraConstuctionHelpers::conditional(LogicalExprPtr&& c0, AlgebraicExprPtr&& r0, AlgebraicExprPtr&& r1) {
	// initializer lists trying to copy?
	//return conditional(LogicalExprList{ std::move(c0) }, AlgebraicExprList{ std::move(r0),  std::move(r1) });
	LogicalExprList conditions;
	conditions.push_back(std::move(c0));
	AlgebraicExprList results;
	results.push_back(std::move(r0));
	results.push_back(std::move(r1));
	return conditional(std::move(conditions), std::move(results));
}

LogicalExprPtr AlgebraConstuctionHelpers::equals(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs){
	return std::make_unique<EqualExpr>(std::move(lhs), std::move(rhs));
}