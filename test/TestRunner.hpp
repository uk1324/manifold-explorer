#pragma once

#include <algebra/Parsing/Scanner.hpp>
#include <algebra/Parsing/Parser.hpp>
#include <algebra/Algebra/Context.hpp>
#include <RefOptional.hpp>
#include "TestingScannerMessageHandler.hpp"
#include "TestingParserMessageHandler.hpp"
#include <span>
#include <sstream>

struct TestRunner {
	TestRunner();

	Scanner scanner;
	TestingScannerMessageHandler scannerMessageHandler;
	Parser parser;
	TestingParserMessageReporter parserMessageHandler;

	Algebra::Context context;

	std::optional<const List<Token>&> tryTokenize(
		std::string_view name,
		std::string_view source, 
		const std::vector<std::string>& variables, 
		const std::vector<std::string>& functions);

	std::optional<const Ast::Expr*> tryParse(
		std::string_view name,
		std::string_view source,
		const List<Token>& tokens,
		const std::vector<std::string>& variables,
		const std::vector<std::string>& functions
	);

	std::optional<Algebra::AlgebraicExprPtr> tryMakeAlgebraicExpr(
		std::string_view name,
		const Ast::Expr* ast
	);

	std::optional<Algebra::AlgebraicExprPtr> tryCompileSourceToAlgebraicExpr(
		std::string_view name,
		std::string_view source);

	std::stringstream output;

	bool printInNotation = true;
	void printExpr(const Algebra::AlgebraicExprPtr& expr);

	void expectedEquals(std::string_view name, const Algebra::AlgebraicExprPtr& got, const Algebra::AlgebraicExprPtr& expected);

	void printPassed(std::string_view name);
	void printFailed(std::string_view name);

	void expectedErrors(
		std::string_view name,
		std::string_view source,
		const std::vector<ScannerError>& expectedScannerErrors,
		const std::vector<ParserError>& expectedParserErrors,
		const std::vector<std::string>& variables = std::vector<std::string>(),
		const std::vector<std::string>& functions = std::vector<std::string>()
	);

	// Istead of creating the desired output (tokens, ast) could instead have a function visit the output and check each part.

	void testScannerOutput(
		std::string_view name,
		std::string_view source,
		const std::vector<Token>& expectedOutput,
		const std::vector<std::string>& variables = std::vector<std::string>(),
		const std::vector<std::string>& functions = std::vector<std::string>());

	void testParserOutput(
		std::string_view name,
		std::string_view source,
		const Ast::Expr* expectedOutput,
		const std::vector<std::string>& variables = std::vector<std::string>(),
		const std::vector<std::string>& functions = std::vector<std::string>());

	i32 failedTimes = 0;

	void reset();
};