#pragma once

#include <string_view>
#include "SourceInfo.hpp"	

static const char* dervativeFunctionName = "D";

enum class TokenType {
	FLOAT,
	PLUS,
	MINUS,
	STAR,
	SLASH,
	CARET,
	COMMA,
	LEFT_PAREN,
	RIGHT_PAREN,
	VARIABLE,
	FUNCTION,
	END_OF_SOURCE,
	WHITESPACE,
	ERROR,
};

const char TOKEN_LEGAL_CHARACTERS[] = "123456789.+-/*()_"
"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

struct Token {
	Token(TokenType type, const SourceLocation& sourceLocation);

	bool operator==(const Token&) const = default;

	i64 start() const;
	i64 end() const;
	i64 length() const;

	// Changed this from storing a string_view to stroring SourceLocation, becsause to get a SourceLocation from a string_view you need access to the source. I think it might be better to store SourceLocations rather than pointers to the source using a string_view
	SourceLocation location;
	TokenType type;
};

const char* tokenTypeToStr(TokenType type);