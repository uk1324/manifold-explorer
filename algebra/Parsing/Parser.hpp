#pragma once

#include "Ast.hpp"
#include "AstAllocator.hpp"
#include "Token.hpp"
#include <List.hpp>
#include "ParserMessageHandler.hpp"
#include <vector>
#include <optional>

struct Parser {
	struct Error {};

	Parser();
	void initialize(
		const List<Token>* tokens,
		std::string_view source,
		ParserMessageHandler* messageHandler);

	// TODO: Use span.
	std::optional<const Expr*> parse(
		const List<Token>& tokens,
		std::string_view source,
		ParserMessageHandler& messageHandler);
	Expr* expr();
	Expr* binaryExpr();
	Expr* plusOrMinusBinaryExpr();
	Expr* timesOrDivideBinaryExpr();
	struct LhsOfBinaryExpr {
		i64 start;
		Expr* lhs;
		BinaryOpType op;
	};
	struct PrimaryExprResult {
		Expr* expr;
		bool absorbedLhs;
	};
	PrimaryExprResult primaryExpr(std::optional<LhsOfBinaryExpr> binaryExprLhs = std::nullopt);
	Expr* exponentiationExpr(Expr* lhs, i64 start);
	Expr* function(std::string_view name, i64 start);

	const Token& peek();
	const Token& peekPrevious();
	bool check(TokenType type);
	bool match(TokenType type, bool ignoreWhitespace = true);
	void expect(TokenType type);
	void advance();
	bool eof();
	[[noreturn]] void throwError(const ParserError& error);

	std::string_view tokenSource(const Token& token) const;

	const List<Token>* tokens;
	i64 currentTokenIndex;

	ParserMessageHandler* messageHandler;
	std::string_view source;
	AstAllocator astAllocator;
};