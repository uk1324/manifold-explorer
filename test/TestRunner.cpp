#include "TestRunner.hpp"
#include "Utils.hpp"
#include <algebra/Algebra/Simplification.hpp>
#include <Put.hpp>
#include <algebra/Parsing/AstEquals.hpp>
#include <algebra/Parsing/AstPrint.hpp>
#include <algebra/PrintingUtils.hpp>

template<typename T>
std::vector<T> setDifference(const std::vector<T>& as, const std::vector<T>& bs) {
	std::vector<T> difference;
	for (const auto& a : as) {
		bool aInBs = false;
		for (const auto& b : bs) {
			if (a == b) {
				aInBs = true;
				break;
			}
		}
		if (!aInBs) {
			difference.push_back(a);
		}
	}
	return difference;
}

TestRunner::TestRunner()
	: scannerMessageHandler(output)
	, parserMessageHandler(output)
{}

std::optional<const List<Token>&> TestRunner::tryTokenize(
	std::string_view name,
	std::string_view source, 
	const std::vector<std::string>& variables, 
	const std::vector<std::string>& functions) {
	
	const auto& tokens = scanner.parse(source, constView(functions), constView(variables), scannerMessageHandler);
	if (scannerMessageHandler.errors.size() != 0) {
		printFailed(name);
		put("scanner error:");
		put("%", output.str());
		return std::nullopt;
	}
	return tokens;
}

std::optional<const Ast::Expr*> TestRunner::tryParse(
	std::string_view name, 
	std::string_view source,
	const List<Token>& tokens, 
	const std::vector<std::string>& variables, 
	const std::vector<std::string>& functions) {

	const auto& ast = parser.parse(tokens, source, parserMessageHandler);
	if (parserMessageHandler.errors.size() != 0) {
		printFailed(name);
		put("parser error:");
		put("%", output.str());
		return std::nullopt;
	}
	return ast;

}

#include <algebra/Algebra/AstToExpr.hpp>

std::optional<Algebra::AlgebraicExprPtr> TestRunner::tryMakeAlgebraicExpr(std::string_view name, const Ast::Expr* ast) {
	auto expr = astToExpr(context, ast);
	if (!expr.has_value()) {
		printFailed(name);
		putAstToExprError(std::cerr, expr.error());
		return std::nullopt;
	}
	return std::move(*expr);
}

std::optional<Algebra::AlgebraicExprPtr> TestRunner::tryCompileSourceToAlgebraicExpr(std::string_view name, std::string_view source) {

	std::vector<std::string> variables;
	std::vector<std::string> functions;
	for (const auto& symbol : context.symbols) {
		variables.push_back(std::string(symbol->name));
	}
	for (const auto& function : context.functions) {
		functions.push_back(std::string(function->name));
	}
	functions.push_back("sqrt");

	auto tokens = tryTokenize(name, source, variables, functions);
	if (!tokens.has_value()) {
		return std::nullopt;
	}
	auto ast = tryParse(name, source, *tokens, variables, functions);
	if (!ast.has_value()) {
		return std::nullopt;
	}
	auto expr = tryMakeAlgebraicExpr(name, *ast);
	if (!expr.has_value()) {
		return std::nullopt;
	}
	return Algebra::basicSimplifiy(context, *expr);
}

#include <algebra/Algebra/PrintExpr.hpp>

void TestRunner::printExpr(const Algebra::AlgebraicExprPtr& expr) {
	if (printInNotation) {
		printAlgebraicExprUsingNotation(expr);
		put("");
	} else {
		printAlgebraicExpr(expr);
	}
}

#include <algebra/Algebra/ConstructionHelpers.hpp>

void TestRunner::expectedEquals(std::string_view name, const Algebra::AlgebraicExprPtr& got, const Algebra::AlgebraicExprPtr& expected) {

	const auto difference = Algebra::basicSimplifiy(context, AlgebraConstuctionHelpers::difference(Algebra::algebraicExprClone(context, got), Algebra::algebraicExprClone(context, expected)));

	if (Algebra::algebraicExprEquals(difference, AlgebraConstuctionHelpers::integer(0))) {
		printPassed(name);
	} else {
		printFailed(name);
		put("expected:");
		printExpr(Algebra::basicSimplifiy(context, expected));

		put("got:");
		printExpr(Algebra::basicSimplifiy(context, got));

		put("simplified difference:");
		printExpr(Algebra::basicSimplifiy(context, difference));
	}
}

void TestRunner::printPassed(std::string_view name) {
	put(TERMINAL_COLOR_GREEN "[PASSED] " TERMINAL_COLOR_RESET "%", name);
}

void TestRunner::printFailed(std::string_view name) {
	failedTimes++;
	put(TERMINAL_COLOR_RED "[FAILED] " TERMINAL_COLOR_RESET "%", name);
}

void TestRunner::expectedErrors(
	std::string_view name, 
	std::string_view source, 
	const std::vector<ScannerError>& expectedScannerErrors,
	const std::vector<ParserError>& expectedParserErrors,
	const std::vector<std::string>& variables,
	const std::vector<std::string>& functions) {

	reset();

	const auto& tokens = scanner.parse(source, constView(functions), constView(variables), scannerMessageHandler);

	const auto scannerErrorsThatWereNotReported = setDifference(expectedScannerErrors, scannerMessageHandler.errors);
	if (scannerErrorsThatWereNotReported.size() != 0) {
		printFailed(name);
		put("% scanner errors were not reported", scannerErrorsThatWereNotReported.size());
		put("output: \n%", output.str());
		return;
	}

	auto ast = parser.parse(tokens, source, parserMessageHandler);
	if (scannerMessageHandler.errors.size() == 0 && !ast.has_value() && parserMessageHandler.errors.size() == 0) {
		ASSERT_NOT_REACHED();
		return;
	}

	const auto parserErrorsThatWereNotReported = setDifference(expectedParserErrors, parserMessageHandler.errors);
	if (parserErrorsThatWereNotReported.size() != 0) {
		printFailed(name);
		put("% parser errors were not reported", parserErrorsThatWereNotReported.size());
		put("output: \n%", output.str());
		return;
	}

	printPassed(name);
}

void TestRunner::testScannerOutput(
	std::string_view name, 
	std::string_view source, 
	const std::vector<Token>& expectedOutput, 
	const std::vector<std::string>& variables, 
	const std::vector<std::string>& functions) {
	
	reset();

	auto tokens = tryTokenize(name, source, variables, functions);
	if (!tokens.has_value()) {
		return;
	}

	auto printWrongOutputMessage = [&]() {
		printFailed(name);
		put("expected:");
		printTokens(source, constView(expectedOutput));
		put("");
		put("got:");
		printTokens(source, constView(*tokens));
		put("");
	};

	if (tokens->size() != expectedOutput.size()) {
		printWrongOutputMessage();
		return;
	}
	for (i32 i = 0; i < tokens->size(); i++) {
		if ((*tokens)[i] != expectedOutput[i]) {
			printWrongOutputMessage();
			return;
		}
	}

	printPassed(name);
}

void TestRunner::reset() {
	output.str("");
}

void TestRunner::testParserOutput(std::string_view name, std::string_view source, const Ast::Expr* expectedOutput, const std::vector<std::string>& variables, const std::vector<std::string>& functions) {
	reset();

	auto tokens = tryTokenize(name, source, variables, functions);
	if (!tokens.has_value()) {
		return;
	}
	auto ast = tryParse(name, source, *tokens, variables, functions);
	if (!ast.has_value()) {
		return;
	}

	if (!astEquals(*ast, expectedOutput, false)) {
		printFailed(name);
		put("expected:");
		astPrint(expectedOutput);
		put("");
		put("got:");
		astPrint(*ast);
		put("");
		return;
	}

	printPassed(name);
}

