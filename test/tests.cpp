#include "TestRunner.hpp"
#include <algebra/Algebra/ConstructionHelpers.hpp>

TestRunner t;

void scannerErrorTests() {
	t.expectedErrors(
		"illegal character",
		"?2 + 2",
		{ IllegalCharScannerError{.character = '?', .sourceOffset = 0 } },
		{}
	);

	t.expectedErrors(
		"invalid identifier",
		" abc",
		{ InvalidIdentifierScannerError{.identifier = "abc", .location = SourceLocation::fromStartLength(1, 3)} },
		{}
	);

	t.expectedErrors(
		"invalid identifier after valid identifier",
		"xabc",
		{ InvalidIdentifierScannerError{.identifier = "abc", .location = SourceLocation::fromStartLength(1, 3)} },
		{},
		{ "x" }
	);
}

void scannerTests() {
	// Not sure if it's better to make to generate the source automatically from tokens to write both the tokens and source.
	std::vector<Token> tokens;
	std::string source;

	auto token = [&](TokenType type, std::string_view text) {
		tokens.push_back(Token(type, SourceLocation::fromStartLength(source.length(), text.length())));
		source += text;
	};

	auto whitespace = [&](i32 length) {
		const auto start = source.length();
		for (i32 i = 0; i < length; i++) {
			source += " ";
		}
		tokens.push_back(Token(TokenType::WHITESPACE, SourceLocation::fromStartLength(start, length)));
	};

	auto endOfSource = [&]() {
		tokens.push_back(Token(TokenType::END_OF_SOURCE, SourceLocation::fromStartLength(source.length(), 0)));
	};

	auto reset = [&]() {
		tokens.clear();
		source.clear();
	};

	using enum TokenType;

	{
		reset();
		token(VARIABLE, "x0");
		token(VARIABLE, "x1");
		endOfSource();

		t.testScannerOutput(
			"identifiers with numbers without whitespace inbetween",
			source,
			tokens,
			{ "x0", "x1" }
		);
	}
}

void parserErrorTests() {
	t.expectedErrors(
		"expected token",
		"(2 + 2",
		{},
		{
			ExpectedTokenParserError{
				.expected = TokenType::RIGHT_PAREN,
				.found = Token(TokenType::END_OF_SOURCE, SourceLocation::fromStartLength(6, 0))
			}
		}
	);

	// This assumes that the last char is the last char in source of length 0.
	t.expectedErrors(
		"unexpected token",
		"(2 + ",
		{},
		{
			UnexpectedTokenParserError{
				.token = Token(TokenType::END_OF_SOURCE, SourceLocation::fromStartLength(5, 0))
			}
		}
	);

}

void parserTests() {
	AstAllocator allocator;
	using namespace Ast;

	using enum BinaryOpType;
	auto binary = [&](BinaryOpType type, Expr* lhs, Expr* rhs) {
		return allocator.allocate<BinaryExpr>(lhs, rhs, type, 0, 0);
	};

	using enum UnaryOpType;
	auto unary = [&](UnaryOpType type, Expr* operand) {
		return allocator.allocate<UnaryExpr>(operand, type, 0, 0);
	};

	auto constant = [&](IntType numerator, IntType denominator = 1) {
		return allocator.allocate<ConstantExpr>(numerator, denominator, 0, 0);
	};

	auto identifier = [&](std::string_view name) {
		return allocator.allocate<IdentifierExpr>(name, 0, 0);
	};

	auto function = [&](std::string_view name, const std::vector<Expr*>& arguments) {
		AstAllocator::List<Expr*> list;
		for (i32 i = 0; i < arguments.size(); i++) {
			allocator.listAppend(list, arguments[i]);
		}
		return allocator.allocate<FunctionExpr>(name, list.span(), 0, 0);
	};

	auto reset = [&]() {
		allocator.reset();
	};

	{
		reset();
		t.testParserOutput(
			"multiplication precedence",
			"1 + 2 * 3",
			binary(ADD,
				constant(1),
				binary(MULTIPLY,
					constant(2),
					constant(3)
				)
			)
		);
	}
	{
		reset();
		t.testParserOutput(
			"1/3x -> 1/(3x)",
			"1/3x",
			binary(DIVIDE,
				constant(1),
				binary(MULTIPLY,
					constant(3),
					identifier("x")
				)
			),
			{ "x" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"1/3 x -> (1/3)x",
			"1/3 x",
			binary(MULTIPLY,
				binary(DIVIDE,
					constant(1),
					constant(3)
				),
				identifier("x")
			),
			{ "x" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"xy^z -> x(y^z)",
			"xy^z",
			binary(MULTIPLY,
				identifier("x"),
				binary(EXPONENTIATE,
					identifier("y"),
					identifier("z")
				)
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x^y^z -> x^(y^z)",
			"x^y^z",
			binary(EXPONENTIATE,
				identifier("x"),
				binary(EXPONENTIATE,
					identifier("y"),
					identifier("z")
				)
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x^ay^b -> x^(a(y^b))",
			"x^ay^b",
			binary(EXPONENTIATE,
				identifier("x"),
				binary(MULTIPLY,
					identifier("a"),
					binary(EXPONENTIATE,
						identifier("y"),
						identifier("b")
					)
				)
				
			),
			{ "x", "y", "a", "b" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x^a y^b -> (x^a)(y^b)",
			"x^a y^b",
				binary(MULTIPLY,
				binary(EXPONENTIATE,
				identifier("x"),
				identifier("a")
				),
					binary(EXPONENTIATE,
						identifier("y"),
						identifier("b")
					)
				),
			{ "x", "y", "a", "b" }
			);
	}
	{
		reset();
		t.testParserOutput(
			"x/y/z -> (x/y)/z",
			"x/y/z",
			binary(DIVIDE,
				binary(DIVIDE,
					identifier("x"),
					identifier("y")
				),
				identifier("z")
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x/yz -> x/(yz)",
			"x/yz",
			binary(DIVIDE,
				identifier("x"),
				binary(MULTIPLY,
					identifier("y"),
					identifier("z")
				)
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x/y z -> (x/y)z",
			"x/y z",
			binary(MULTIPLY,
				binary(DIVIDE,
					identifier("x"),
					identifier("y")
				),
				identifier("z")
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"a/b / c/d -> ((a/b)/c/d))",
			"a/b / c/d",
			binary(DIVIDE,
				binary(DIVIDE,
					binary(DIVIDE,
						identifier("a"),
						identifier("b")
					),
					identifier("c")
				),
				identifier("d")
			),
			{ "a", "b", "c", "d" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"x^y/z -> (x^y)/z",
			"x^y/z",
			binary(DIVIDE,
				binary(EXPONENTIATE,
					identifier("x"),
					identifier("y")
				),
				identifier("z")
			),
			{ "x", "y", "z" }
		);
	}
	{
		reset();
		t.testParserOutput(
			"1 2^x -> 1 * 2^x",
			"1 2^x",
			binary(MULTIPLY,
				constant(1),
				binary(EXPONENTIATE,
					constant(2),
					identifier("x")
				)
			),
			{ "x" }
		);
	}
}

#include <algebra/Algebra/Simplification.hpp>
#include <algebra/Algebra/Context.hpp>
#include <list>

namespace Symbols {

using namespace Algebra;

Symbol* addVariable(std::string&& variableName) {
	return t.context.addVariable(std::move(variableName));
}

const auto a = addVariable("a");
const auto b = addVariable("b");
const auto c = addVariable("c");
const auto d = addVariable("d");
const auto e = addVariable("e");
const auto f = addVariable("f");
const auto x = addVariable("x");
const auto y = addVariable("y");
const auto z = addVariable("z");
const auto x1 = addVariable("x1");
const auto x2 = addVariable("x2");
const auto xa = addVariable("xa");

}

void algebraicExpressionLessThanTests() {
	using namespace AlgebraConstuctionHelpers;

	auto test = [](std::string_view name, const AlgebraicExprPtr& a, const AlgebraicExprPtr& b) {
		if (algebraicExprLessThan(t.context, a, b)) {
			t.printPassed(name);
		} else {
			t.printFailed(name);
		}
	};
	using namespace Symbols;

	test("2 < 5/2", integer(2), rational(5, 2));
	test("a < b", symbol(a), symbol(b));
	test("x1 < x2", symbol(x1), symbol(x2));
	test("x1 < xa", symbol(x1), symbol(xa));

	test(
		"a + b < a + c",
		sum(symbol(a), symbol(b)),
		sum(symbol(a), symbol(c))
	);

	// Comparasion is evaluated from right to left. The rightmost value is the most significand.
	test(
		"b + a < a + b",
		sum(symbol(b), symbol(a)),
		sum(symbol(a), symbol(b))
	);

	test(
		"a + c + d < b + c + d",
		sum(symbol(a), symbol(c), symbol(d)),
		sum(symbol(b), symbol(c), symbol(d))
	);

	test(
		"c + d < b + c + d",
		sum(symbol(c), symbol(d)),
		sum(symbol(b), symbol(c), symbol(d))
	);

	test(
		"d + c + a < c + b",
		sum(symbol(d), symbol(c), symbol(a)),
		sum(symbol(c), symbol(b))
	);

	test(
		"(1+x)^2 < (1+x)^3",
		power(sum(integer(1), symbol(x)), integer(2)),
		power(sum(integer(1), symbol(x)), integer(3))
	);

	test(
		"(1+x)^3 < (1+y)^2",
		power(sum(integer(1), symbol(x)), integer(3)),
		power(sum(integer(1), symbol(y)), integer(2))
	);

}

#include <Put.hpp>
#include <algebra/Algebra/PrintExpr.hpp>
#include <algebra/Algebra/Simplification.hpp>

void algebraicExpressionSimplifyTests() {
	using namespace AlgebraConstuctionHelpers;
	using namespace Symbols;
	auto test = [](std::string_view name, const AlgebraicExprPtr& toSimplify, const AlgebraicExprPtr& expected) {
		bool printInNotation = false;
		printInNotation = true;
		const auto simplified = basicSimplifiy(t.context, toSimplify);
		auto print = [&printInNotation](const AlgebraicExprPtr& expr) {
			if (printInNotation) {
				printAlgebraicExprUsingNotation(expr);
				put("");
			} else {
				printAlgebraicExpr(expr);
			}
		};

		if (algebraicExprEquals(simplified, expected)) {
			t.printPassed(name);
		} else {
			put("simplified :");
			print(toSimplify);

			t.printFailed(name);
			put("expected:");
			print(expected);
			
			put("got:");
			print(simplified);
		}
	};

	test(
		"x + (x + y)",
		sum(symbol(x), sum(symbol(x), symbol(y))),
		sum(product(integer(2), symbol(x)), symbol(y))
	);

	test(
		"2x + (3/2)x -> (7/2)x",
		sum(
			product(integer(2), symbol(x)),
			product(rational(3, 2), symbol(x))
		),
		product(rational(7, 2), symbol(x))
	);

	test(
		"x + 2x + 3x -> 6x",
		sum(
			symbol(x),
			product(integer(2), symbol(x)),
			product(integer(3), symbol(x))
		),
		product(integer(6), symbol(x))
	);

	test(
		"2x + y + (3/2)x -> (9/2)x + y",
		sum(
			product(integer(2), symbol(x)),
			symbol(y),
			product(rational(3, 2), symbol(x))
		),
		sum(
			product(rational(7, 2), symbol(x)),
			symbol(y)
		)
	);

	test(
		"(a + (b + c)) + (-1)((a + b) + c)",
		sum(
			sum(
				symbol(a),
				sum(symbol(b), symbol(c))
			),
			product(
				integer(-1),
				sum(
					sum(symbol(a), symbol(b)),
					symbol(c)
				)
			)
		),
		integer(0)
	);

	test(
		"x3a -> 3ax",
		product(symbol(x), integer(3), symbol(a)),
		product(integer(3), symbol(a), symbol(x))
	);

	test(
		"xx^(-1) -> 1",
		product(
			symbol(x),
			power(symbol(x), integer(-1))
		),
		integer(1)
	);

	test(
		"2ace3bde -> 6abcde^2",
		product(
			integer(2), 
			symbol(a), 
			symbol(c), 
			symbol(e), 
			integer(3), 
			symbol(b), 
			symbol(d), 
			symbol(e)
		),
		product(
			integer(6),
			symbol(a),
			symbol(b),
			symbol(c),
			symbol(d),
			power(symbol(e), integer(2))
		)
	);

	test(
		"a(bc) + (-1)(ab)c",
		sum(
			product(symbol(a), product(symbol(b), symbol(c))),
			product(
				integer(-1), 
				product(
					product(symbol(a), symbol(b)), 
					symbol(c)
				)
			)
		),
		integer(0)
	);

	test(
		"x x^2 x^3 -> x^6",
		product(
			symbol(x),
			power(symbol(x), integer(2)),
			power(symbol(x), integer(3))
		),
		power(symbol(x), integer(6))
	);

	test(
		"(xy)(xy)^2 -> x^3 y^3",
		product(
			product(symbol(x), symbol(y)),
			power(product(symbol(x), symbol(y)), integer(2))
		),
		product(power(symbol(x), integer(3)), power(symbol(y), integer(3)))
	);

	test(
		"((x^(1/2))^(1/2))^8 -> x^(1/2)",
		power(power(power(symbol(x), rational(1, 2)), rational(1, 2)), integer(8)),
		power(symbol(x), integer(2))
	);

	test(
		"((xy)^(1/2) z^2)^2 -> xyz^4",
		power(
			product(
				power(product(symbol(x), symbol(y)), rational(1, 2)),
				power(symbol(z), integer(2))
			),
			integer(2)
		),
		product(symbol(x), symbol(y), power(symbol(z), integer(4)))
	);

	// There was a bug in simplify product recursive. When the first term of (-1)(x) * (-1)(x) got simplified to 1 simplify product recursive returned [1] instead of []. Which caused the expression to not be fully simplified.
	test(
		"-x * -x * x ^ x -> x^(2 + x)",
		product(
			negate(symbol(x)),
			product(
				negate(symbol(x)),
				power(symbol(x), symbol(x))
			)
		),
		power(symbol(x), sum(integer(2), symbol(x)))
	);

	
}

#include <algebra/Algebra/Derivative.hpp>

void derivativeTests() {
	using namespace AlgebraConstuctionHelpers;
	using namespace Symbols;
	auto test = [](std::string_view name, const Symbol* symbol, std::string_view source, std::string_view expected) {
		const auto gotExpr = t.tryCompileSourceToAlgebraicExpr(name, source);
		if (!gotExpr.has_value()) {
			return std::nullopt;
		}
		const auto expectedExpr = t.tryCompileSourceToAlgebraicExpr(name, expected);
		if (!expectedExpr.has_value()) {
			return std::nullopt;
		}
		const auto derivative = Algebra::derivative(t.context, Algebra::basicSimplifiy(t.context, *gotExpr), symbol);
		t.expectedEquals(name, derivative, *expectedExpr);
	};

	test(
		"D(x, x) = 1",
		x,
		"x",
		"1"
	);

	test(
		"D(y, x) = 0",
		x,
		"y",
		"0"
	);

	test(
		"D(sin(y + z^y), x) = 0",
		x,
		"sin(y + z^y)",
		"0"
	);

	test(
		"D(x^2, x) = 2x",
		x,
		"x^2",
		"2x"
	);

	test(
		"D(x^0, x) = 0",
		x,
		"x^0",
		"0"
	);

	test(
		"D(sin(x), x) = cos(x)",
		x,
		"sin(x)",
		"cos(x)"
	);

	test(
		"D(cos(x), x) = cos(x)",
		x,
		"cos(x)",
		"-sin(x)"
	);

	test(
		"D(ln(x), x) = 1/x",
		x,
		"ln(x)",
		"1/x"
	);

	test(
		"D(sin(cos(x)), x) = -cos(cos(x)) sin(x)",
		x,
		"sin(cos(x))",
		"-cos(cos(x)) sin(x)"
	);

	test(
		"D(e^x, x) = e^x",
		x,
		"e^x",
		"e^x"
	);

	test(
		"D(2^x, x) = ln(2) 2^x",
		x,
		"2^x",
		"ln(2) 2^x"
	);

	test(
		"D(x^(1/2) + x^(-1), x) = (1/2)x^(-1/2) -x^(-2)",
		x,
		"x^(1/2) + x^(-1)",
		"(1/2)x^(-1/2) -x^(-2)"
	);
}

#include <algebra/PrintingUtils.hpp>
#include "randomTestsMain.hpp"

void debugMain();

int main() {
	randomTestsMain();
	return 0;

	bool debug = true;
	//debug = false;
	if (debug) {
		debugMain(); 
	} else {
		scannerErrorTests();
		scannerTests();
		parserErrorTests();
		parserTests();
		algebraicExpressionLessThanTests();
		algebraicExpressionSimplifyTests();
		derivativeTests();

		if (t.failedTimes > 0) {
			put(TERMINAL_COLOR_RED "Failed % tests." TERMINAL_COLOR_RESET, t.failedTimes);
		}
	}
}

#include <iostream>
#include "Utils.hpp"
#include <algebra/Parsing/AstPrint.hpp>
#include <algebra/Algebra/PrintExpr.hpp>
#include <algebra/Algebra/AstToExpr.hpp>
#include <Put.hpp>

void debugMain() {
	Scanner scanner;
	OstreamScannerMessageHandler scannerMessageHandler(std::cout);
	Parser parser;
	TestingParserMessageReporter parserMessageHandler(std::cout);

	bool printTokens = true;
	bool printAst = true;
	bool printAlgebraicExpr = true;

	//std::string_view source = " 4.^     y ^ sin(  (81.) )";
	//std::string_view source = "( -1 * ( 0^( 3^e ) ) )";
	//std::string_view source = "( ( e^e ) * ( e^2 ) )";
	//std::string_view source = "-(e)*-(e)  *  (e) ^(e)";
	std::string_view source = "(4)  -  (z)  - (4.)  -  (z)";
	std::vector<std::string> variables{ "a", "b", "c", "x", "y", "z", "e" };
	std::vector<std::string> functions{ "sin" };

	const auto& tokens = scanner.parse(source, constView(functions), constView(variables), scannerMessageHandler);
	if (printTokens) {
		put("Tokens:");
		::printTokens(source, constView(tokens));
		putnn("\n\n");
	}


	const auto ast = parser.parse(tokens, source, parserMessageHandler);
	if (!ast.has_value()) {
		return;
	}

	if (printAst) {
		put("Ast:");
		astPrint(*ast);
		putnn("\n\n");
	}

	Algebra::Context context;
	
	for (const auto& symbol : variables) {
		context.addVariable(std::string(symbol));
	}
	auto optAlgebraicExpr = astToExpr(context, *ast);
	if (!optAlgebraicExpr.has_value()) {
		putAstToExprError(std::cerr, optAlgebraicExpr.error());
		return;
	}

	auto e = std::move(*optAlgebraicExpr);
	e = Algebra::basicSimplifiy(context, e);
	//e = Algebra::derivative(context, Algebra::basicSimplifiy(context, e), &x);

	put("Algebraic expression:");
	Algebra::printAlgebraicExpr(e);
	putnn("\n\n");

	put("Algebraic expression using notation:");
	Algebra::printAlgebraicExprUsingNotation(e);
	putnn("\n\n");

	Algebra::isSimplifiedExpr(context, e);
}