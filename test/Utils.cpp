#include "Utils.hpp"
#include <Put.hpp>

void printToken(std::string_view source, const Token& token) {
	// substr handles the case where length is too big, but doesn't handle where the start is too big.
	const auto tokenSource = source.substr(std::min(token.start(), i64(source.length()) - 1), token.length());
	putnn("%(\"%\")", tokenTypeToStr(token.type), tokenSource);
};

void printTokens(std::string_view source, const View<const Token> tokens) {
	for (const auto& token : tokens) {
		printToken(source, token);
		putnn(" ");
	}
};