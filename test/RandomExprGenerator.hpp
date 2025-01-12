#pragma once

#include <StringStream.hpp>
#include <View.hpp>
#include <algebra/Algebra/Expr.hpp>
#include <span>
#include <random>

struct RandomInputGenerator {
	RandomInputGenerator();

	std::string_view generate(
		View<const Algebra::Symbol* const> symbols,
		View<const Algebra::Function* const> functions);

	void expr(i32 nesting);
	void nAryExpr(i32 nesting, const char* op, bool addWhitespace = true);
	void constantExpr();
	void symbolExpr();

	// Whitespace should be generated after the first character and before the last.
	// For example this is wrong: '  (a + b)' but this is not '(a   +b )'.
	void whitespace();

	//i32 randomNumber();
	bool randomBool();
	i32 randomFrom0To(i32 x);
	i32 randomInRangeExclusive(i32 includingStart, i32 excludingEnd);
	i32 randomInRangeInclusive(i32 includingStart, i32 includingEnd);
	usize randomIndex(usize size);
	char randomDigit();

	i32 maxNestingDepth = 5;
	i32 whitespaceMaxLength = 3;
	i32 integerPartMaxLength = 1;
	i32 fractionalPartMaxLength = 1;
	i32 maxImplicitMultiplicationChainLength = 5;
	i32 maxArity = 5;

	std::random_device dev;
	std::mt19937 rng;

	StringStream out;
	View<const Algebra::Symbol* const> symbols;
	View<const Algebra::Function* const> functions;
};