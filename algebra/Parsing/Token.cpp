#include "Token.hpp"
#include "Assertions.hpp"

Token::Token(TokenType type, const SourceLocation& sourceLocation)
	: type(type)
	, location(sourceLocation) {}

i64 Token::start() const {
	return location.start;
}

i64 Token::end() const {
	return location.end();
}

i64 Token::length() const {
	return location.length;
}

const char* tokenTypeToStr(TokenType type) {
	switch (type) {
	case TokenType::PLUS: return "PLUS";
	case TokenType::MINUS: return "MINUS";
	case TokenType::FLOAT: return "FLOAT";
	case TokenType::STAR: return "STAR";
	case TokenType::SLASH: return "SLASH";
	case TokenType::CARET: return "CARET";
	case TokenType::LEFT_PAREN: return "LEFT_PAREN";
	case TokenType::RIGHT_PAREN: return "LEFT_PAREN";
	case TokenType::END_OF_SOURCE: return "END_OF_SOURCE";
	case TokenType::VARIABLE: return "VARIABLE";
	case TokenType::FUNCTION: return "FUNCTION";
	case TokenType::ERROR: return "ERROR";
	case TokenType::WHITESPACE: return "WHITESPACE";
	case TokenType::COMMA: return "COMMA";
	}
	ASSERT_NOT_REACHED();
	return nullptr;
}