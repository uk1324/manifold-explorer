#pragma once

#include <Types.hpp>
#include <vector>
#include <memory>
#include <string>

/*
The most generic way to make an expression would be to have
struct Expression {
	string name;
	List<AlgebraicExpr> algebraicExpressions;
	List<LogicalExpr> logicalExpressions
}
One issue with this representation is that if function calls are just expressions like this then you can get into colisions with existing names. I guess you could also have a boolean if this is a reserved identifier.
Another issue is that even if the name is for example "If" you don't know if it actually has all the components of an "If". This issue is still there even if you seprate things into individual types. For example a sum with zero components still obtainable. I guess you could interpret that as zero.

One advantage of the generic way would probably be that you can handle recursion more generically.

I don't think using virtual calls makes sense for this, because you often need to access the original expression anyway. So it's probably better to just store the type inside the base type. I guess you could just have a virtual function that returns the type, but that would be less efficient I think. Because first you would need to do virtual dispatch and then probably use the type for a switch statement.
*/

namespace Algebra {

enum class AlgebraicExprType {
	INTEGER,
	RATIONAL,
	SYMBOL,
	FUNCTION,
	SUM,
	PRODUCT,
	POWER,
};

struct IntegerExpr;
struct RationalExpr;
struct PowerExpr;
struct ProductExpr;
struct SumExpr;

using IntegerType = i64;
struct AlgebraicExpr {
	AlgebraicExpr(AlgebraicExprType type);
	AlgebraicExprType type;

	bool isUndefined() const;
	bool isIntegerValue(IntegerType value) const;
	bool isInteger() const;
	bool isRational() const;
	bool isPower() const;
	bool isProduct() const;
	bool isSum() const;

	// Making this function so it's shorter. Also it can check if the type is correct.
	const IntegerExpr* asInteger() const;
	const RationalExpr* asRational() const;
	const PowerExpr* asPower() const;
	PowerExpr* asPower();
	const ProductExpr* asProduct() const;
	ProductExpr* asProduct();
	const SumExpr* asSum() const;
	SumExpr* asSum();
};

using AlgebraicExprPtr = std::unique_ptr<Algebra::AlgebraicExpr>;
using AlgebraicExprList = std::vector<AlgebraicExprPtr>;


struct IntegerExpr : public AlgebraicExpr {
	IntegerExpr(IntegerType value);

	IntegerType value;
};

struct RationalExpr : public AlgebraicExpr {
	RationalExpr(IntegerType numerator, IntegerType denominator);

	IntegerType numerator;
	IntegerType denominator;
};

struct SymbolExpr : public AlgebraicExpr {
	SymbolExpr(std::string&& name);

	std::string name;
};

struct FunctionExpr : public AlgebraicExpr {
	FunctionExpr(std::string&& name, AlgebraicExprList&& arguments);

	std::string name;
	AlgebraicExprList arguments;
};

// Making the lhs rhs constructors, because I can't use std::vector initialization, because it uses initializer lists which don't handle movable types, because they always copy. Could use varadic arguments, but I don't need them and they are more complicated.

struct SumExpr : public AlgebraicExpr {
	SumExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs);
	SumExpr(AlgebraicExprList&& summands);
	AlgebraicExprList summands;
};

struct ProductExpr : public AlgebraicExpr {
	ProductExpr(AlgebraicExprPtr&& lhs, AlgebraicExprPtr&& rhs);
	ProductExpr(AlgebraicExprList&& factors);
	AlgebraicExprList factors;
};

struct PowerExpr : public AlgebraicExpr {
	PowerExpr(AlgebraicExprPtr&& base, AlgebraicExprPtr&& exponent);
	AlgebraicExprPtr base;
	AlgebraicExprPtr exponent;

	AlgebraicExprPtr clone() const;
};

AlgebraicExprPtr algebraicExprClone(const AlgebraicExprPtr& expr);
AlgebraicExprPtr makeUndefined();

}
