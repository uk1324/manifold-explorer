#include "TestRunner.hpp"

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

	using enum BinaryOpType;
	auto binary = [&](BinaryOpType type, Expr* lhs, Expr* rhs) {
		return allocator.allocate<BinaryExpr>(lhs, rhs, type, 0, 0);
	};

	using enum UnaryOpType;
	auto unary = [&](UnaryOpType type, Expr* operand) {
		return allocator.allocate<UnaryExpr>(operand, type, 0, 0);
	};

	auto constant = [&](FloatType value) {
		return allocator.allocate<ConstantExpr>(value, 0, 0);
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
	}
}

void debugMain();

int main() {
	scannerErrorTests();
	scannerTests();
	parserErrorTests();
	parserTests();
}

#include <iostream>
#include "Utils.hpp"
#include <algebra/Parsing/AstPrint.hpp>

void debugMain() {
	Scanner scanner;
	OstreamScannerMessageHandler scannerMessageHandler(std::cout);
	Parser parser;
	TestingParserMessageReporter parserMessageHandler(std::cout);

	bool printTokens = true;
	bool printAst = true;

	std::string_view source;
	std::vector<std::string> variables{ };
	std::vector<std::string> functions{ };

	const auto& tokens = scanner.parse(source, constView(functions), constView(variables), scannerMessageHandler);
	if (printTokens) {
		::printTokens(source, constView(tokens));
	}

	const auto ast = parser.parse(tokens, source, parserMessageHandler);
	if (!ast.has_value()) {
		return;
	}

	if (printAst) {
		astPrint(*ast);
	}
}