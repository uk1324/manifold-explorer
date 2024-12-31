#pragma once

#include <Types.hpp>
#include "ScannerMessageHandler.hpp"
#include "Token.hpp"
#include <vector>
#include <optional>
#include <string_view>
#include <List.hpp>
#include <View.hpp>

struct Scanner {
	struct Error {

	};

	Scanner();

	void initialize(
		std::string_view source,
		View<const std::string> functionNames,
		View<const std::string> variableNames,
		ScannerMessageHandler* messageHandler);

	const List<Token>& parse(
		std::string_view source,
		View<const std::string> functionNames,
		View<const std::string> variableNames,
		ScannerMessageHandler& messageHandler);

	Token token();
	Token number();
	Token identifier(u8 firstChar);

	u8 peek();
	bool match(char c);
	std::optional<Token> skipWhitespace();
	void advance();
	bool eof();
	Token makeToken(TokenType type);
	Token error(const ScannerError& error);

	static bool isDigit(u8 c);
	static bool isAlpha(u8 c);

	List<Token> tokens;

	i64 currentTokenStartIndex;
	i64 currentCharIndex;
	std::string_view source;

	View<const std::string> functionNames;
	View<const std::string> variableNames;

	ScannerMessageHandler* messageHandler;
};