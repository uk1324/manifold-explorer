#include "ConstructionHelpers.hpp"
#include "Functions.hpp"

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