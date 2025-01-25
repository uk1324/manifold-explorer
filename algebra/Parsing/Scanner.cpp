#include "Scanner.hpp"
#include "../StringUtils.hpp"
#include "CharUtils.hpp"
#include <Assertions.hpp>
#include <charconv>
#include <optional>

Scanner::Scanner() 
	: tokens(List<Token>::empty())
	, functionNames(View<const std::string>::empty()) 
	, variableNames(View<const std::string>::empty()) {
	initialize("", View<const std::string>::empty(), View<const std::string>::empty(), nullptr);
}

void Scanner::initialize(
	std::string_view source,
	View<const std::string> functionNames,
	View<const std::string> variableNames,
	ScannerMessageHandler* messageHandler) {

	currentCharIndex = 0;
	currentTokenStartIndex = 0;
	this->source = source;
	this->messageHandler = messageHandler;
	if (messageHandler != nullptr) {
		messageHandler->initialize(source);
	}
	this->functionNames = functionNames;
	this->variableNames = variableNames;
	tokens.clear();
}

const List<Token>& Scanner::parse(
	std::string_view source,
	View<const std::string> functionNames,
	View<const std::string> variableNames,
	ScannerMessageHandler& messageHandler) {

	initialize(source, functionNames, variableNames, &messageHandler);

	while (!eof()) {
		try {
			const auto result = skipWhitespace();
			if (eof()) {
				break;
			}
			if (result.has_value()) {
				tokens.add(*result);
			}
			tokens.add(token());
		} catch (const Error&) {

		}
	}
	currentTokenStartIndex = currentCharIndex;
	tokens.add(makeToken(TokenType::END_OF_SOURCE));

	return tokens;
}

Token Scanner::token() {
	u8 c = peek();
	advance();

	switch (c) {
	case '+': return makeToken(TokenType::PLUS);
	case '-': return makeToken(TokenType::MINUS);
	case '*': return makeToken(TokenType::STAR);
	case '/': return makeToken(TokenType::SLASH);
	case '^': return makeToken(TokenType::CARET);

	case ',': return makeToken(TokenType::COMMA);

	case '(': return makeToken(TokenType::LEFT_PAREN);
	case ')': return makeToken(TokenType::RIGHT_PAREN);

	default:
		// TODO: Should numbers like 0001 be allowed?
		if (isDigit(c)) {
			return number();
		}
		if (isAlpha(c)) {
			return identifier(c);
		}
		break;
	}
	ASSERT(currentCharIndex >= 0);
	return error(IllegalCharScannerError{
		.character = c,
		.sourceOffset = currentCharIndex - 1
	});
}

Token Scanner::number() {
	while (!eof()) {
		if (match('.')) {
			while (!eof() && isDigit(peek())) {
				advance();
			}
			break;
		} else if (!isDigit(peek())) {
			break;
		}
		advance();
	}
	return makeToken(TokenType::FLOAT);
}

Token Scanner::identifier(u8 firstChar) {
	auto isVariableChar = [](char c) -> bool {
		return isAlpha(c) || isDigit(c) || (c == '_');
		};

	while (!eof() && isVariableChar(peek())) {
		advance();
	}

	const auto tokenSource = source.substr(currentTokenStartIndex, currentCharIndex - currentTokenStartIndex);
	struct Prefix {
		TokenType type;
		std::string_view text;
	};
	// The longest prefix is chosen so for example cos(x) can't be parsed as c * os(x)
	std::optional<Prefix> longestPrefix;

	// @Performance
	auto checkPrefix = [&](std::string_view possiblePrefix, TokenType type) {
		if (!isPrefix(tokenSource, possiblePrefix)) {
			return;
		}

		Prefix prefix{
			.type = type,
			.text = possiblePrefix
		};
		if (longestPrefix.has_value()) {
			if (prefix.text.length() > longestPrefix->text.length()) {
				longestPrefix = prefix;
			}
		} else {
			longestPrefix = prefix;
		}
	};

	for (const auto& name : functionNames) {
		checkPrefix(name, TokenType::FUNCTION);
	}
	checkPrefix(dervativeFunctionName, TokenType::FUNCTION);
	for (const auto& name : variableNames) {
		checkPrefix(name, TokenType::VARIABLE);
	}

	if (!longestPrefix.has_value()) {
		return error(InvalidIdentifierScannerError{
			.identifier = tokenSource,
			.location = SourceLocation::fromStartEnd(currentTokenStartIndex, currentCharIndex)
		});
	}

	// TODO: Because of this if there is an empty variable name then this becomes an infinite loop adding more and more variable tokens.
	currentCharIndex = currentTokenStartIndex + longestPrefix->text.length();
	return makeToken(longestPrefix->type);
}

u8 Scanner::peek() {
	if (currentCharIndex >= static_cast<i64>(source.size())) {
		return '\0';
	}
	ASSERT(currentCharIndex >= 0);
	return source[static_cast<usize>(currentCharIndex)];
}

bool Scanner::match(char c) {
	if (peek() == c) {
		advance();
		return true;
	}
	return false;
}

std::optional<Token> Scanner::skipWhitespace() {
	while (!eof()) {
		const auto c = peek();
		switch (c) {
		case ' ':
			advance();
			break;

		default:
			if (currentTokenStartIndex != currentCharIndex) {
				return makeToken(TokenType::WHITESPACE);
			}
			return std::nullopt;
		}
	}
	return std::nullopt;
}

void Scanner::advance() {
	if (currentCharIndex < static_cast<i64>(source.size())) {
		currentCharIndex++;
	}
}

bool Scanner::eof() {
	return currentCharIndex >= static_cast<i64>(source.size());
}

Token Scanner::makeToken(TokenType type) {
	Token token(type, SourceLocation::fromStartEnd(currentTokenStartIndex, currentCharIndex));
	//put("'%'", source.substr(currentTokenStartIndex, currentCharIndex - currentTokenStartIndex));
	currentTokenStartIndex = currentCharIndex;
	return token;
}

Token Scanner::error(const ScannerError& error) {
	messageHandler->onError(error);
	return makeToken(TokenType::ERROR);
}