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
	DERIVATIVE
};
struct Context;
struct IntegerExpr;
struct RationalExpr;
struct PowerExpr;
struct ProductExpr;
struct SumExpr;
struct SymbolExpr;
struct FunctionExpr;
struct DerivativeExpr;
struct Symbol;

using IntegerType = i64;
struct AlgebraicExpr {
	AlgebraicExpr(AlgebraicExprType type);
	virtual ~AlgebraicExpr() {};
	AlgebraicExprType type;
	// TODO: Implement a destructor that switches based on the type.
	// unique_ptr requires expanding twice to get to the value in the debugger. Could implement a custom unique ptr. Could call it OwnPtr and have a function ownPtr<T> that construct it. Or maybe a constructor would work.
	// Also could implement automatic visualization of the expression in the debugger. 

	bool isUndefined() const;
	bool isIntegerValue(IntegerType value) const;
	bool isInteger() const;
	bool isRational() const;
	bool isPower() const;
	bool isProduct() const;
	bool isSum() const;
	bool isSymbol() const;
	bool isFunction() const;
	bool isDerivative() const;

	// Making this function so it's shorter. Also it can check if the type is correct.
	const IntegerExpr* asInteger() const;
	const RationalExpr* asRational() const;
	const PowerExpr* asPower() const;
	PowerExpr* asPower();
	const ProductExpr* asProduct() const;
	ProductExpr* asProduct();
	const SumExpr* asSum() const;
	SumExpr* asSum();
	const SymbolExpr* asSymbol() const;
	SymbolExpr* asSymbol();
	const FunctionExpr* asFunction() const;
	FunctionExpr* asFunction();
	const DerivativeExpr* asDerivative() const;
	DerivativeExpr* asDerivative();

	bool isFreeOfVariable(const Symbol* variable) const;
};

using AlgebraicExprPtr = std::unique_ptr<Algebra::AlgebraicExpr>;
using AlgebraicExprList = std::vector<AlgebraicExprPtr>;

// Using an enum to have an easy and deterministic way of comparing function symbols. If dynamic types are used then you need to handle cases where both names are equal, but the symbols aren't (this can break the sorting in the simplification algorithm). If you just used the pointers it wouldn't be determinstic. It would depend on the way those symbols were allocated. 
// Duplicate names can happen for example in differential equation (duplicated constant names), but just doing the comparasion with the types won't help here so I don't know what to do but to resort to some global counting (which would be allocation order dependent) or to just use pointer comparasions.
enum class FunctionType {
	SIN, COS, LN
};

struct Function {
	/*
	The function arguments are differently bound that variables
	Could have a default set of bound variables. For example x, y, z for function with arity 3 and maybe
	x0, ..., xn for arity n
	*/
	Function(FunctionType type, std::string&& name, i32 arity);

	//// TODO: Maybe provide default values that just return the function exeucted on those arguments.
	//// arguments.size() is always equal to arity
	//virtual AlgebraicExprPtr simplify(Context* c, const AlgebraicExprList& arguments) = 0;
	//// Thid doesn't return a function to simplify handling cases like ln(x) -> 1/x.
	//virtual AlgebraicExprPtr evaluatePartialDerivativeOn(Context* c, const AlgebraicExprPtr& value, i32 partialDerivativeIndex) = 0;

	FunctionType type;
	const std::string name;
	const i32 arity;
};

enum class SymbolType {
	UNDEFINED,
	VARIABLE,
	PI,
	E,
};

struct Symbol {
	Symbol(SymbolType type, std::string&& name);

	SymbolType type;
	std::string name;
};

struct UndefinedSymbol final : public Symbol {
	UndefinedSymbol();
};

struct VariableSymbol final : public Symbol {
	VariableSymbol(std::string&& name);
};

struct EulersNumberSymbol final : public Symbol {
	EulersNumberSymbol();
};

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
	SymbolExpr(const Symbol* symbol);

	const Symbol* symbol;
};

struct FunctionExpr : public AlgebraicExpr {
	FunctionExpr(const Function* function, AlgebraicExprList&& arguments);

	// I know this is bad a introduces a lot of copies, but at the moment I also don't know what the best way to deal with it would be. Could have shared pointers (this would still lead to copies in cases like a derivative of sine returning a newly allocated cosine function, this could be fixed by the context), could have a context that stores the pointers to the defined functions and have a lookup there (this is basically what I implemented in the garbage collector but functions instead of strings). I am thinking if the best way to deal with this is to implement a garbage collector.
	// Also comparasion of functions would be easier if they were shared.
	// Also if you have a function with a matrix argument then that is different from the function with a number argument.
	// If the function isn't in the context then it can be added when needed. For example if sin is in the context, but consine isn't then the derivative of sine would add cos to the context. This would be done automatically if an allocator like discribed above was used.
	/*
	Local symbols may be useful.
	For example when solving a second order differential equation there are cases when you need to recursively solve a first order order equation and then put the result into another first order equation. This creates constants that probably have the same name. Could instead return a list of local variables. The variables wouldn't be recognized by their string name, but by their pointers.
	The issue with comparasion by pointers is that functions need to access other symbols. For example differentiation of sin needs to access cos.
	Could make a singleton for these types, but that would be global to the whole program.
	There are also global constants like e or pi.
	If the globals had to be shared another issue is that if you made constant that depend on parameters. Like for example the zeros of the bessel function then each would need to be allocated and shared. I guess it still uses less space, but a search would need to be done each time.
	*/
	/*
	To check if 2 symbols are equal a dynamic cast must be used.
	I guess if you implemented it without a dynamic cast you would need some pointer to a vtable anyway. 

	FunctionPtr function;
	*/
	const Function* function;
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

	AlgebraicExprPtr clone(const Context& c) const;
};

struct DerivativeExpr : public AlgebraicExpr {
	DerivativeExpr(AlgebraicExprPtr&& expr, const Symbol* symbol);
	AlgebraicExprPtr expr;
	const Symbol* symbol;
};

AlgebraicExprPtr algebraicExprClone(const Context& c, const AlgebraicExprPtr& expr);
AlgebraicExprList algebraicExprListClone(const Context& c, const AlgebraicExprList& list);

}
