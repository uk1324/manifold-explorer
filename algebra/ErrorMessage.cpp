#include "errorMessage.hpp"
#include <variant>
#include <Overloaded.hpp>
#include "PrintingUtils.hpp"
#include "Parsing/Token.hpp"
#include <Put.hpp>

void outputScannerErrorMessage(std::ostream& out, const ScannerError& error, std::string_view source, bool printLocation) {
	std::visit(overloaded{
		[&](const IllegalCharScannerError& e) {
			putnn(out, "illegal character '%', code = '%'", static_cast<char>(e.character), static_cast<int>(e.character));
			if (printLocation) {
				out << '\n';
				highlightInText(out, source, e.sourceOffset, 1);
				out << '\n';
			}
		},
		[&](const InvalidIdentifierScannerError& e) {
			putnn(out, "'%' is not a valid function or variable", e.identifier);
			if (printLocation) {
				out << '\n';
				highlightInText(out, source, e.location.start, e.location.length);
				out << '\n';
			}
		}
	}, error);
}

static const char* tokenTypeName(TokenType type) {
	switch (type) {
	case TokenType::FLOAT: return "floating point constant";
	case TokenType::PLUS: return "'+'";
	case TokenType::MINUS: return "'-'";
	case TokenType::STAR: return "'*'";
	case TokenType::SLASH: return "'/'";
	case TokenType::CARET: return "'^'";
	case TokenType::COMMA: return "','";
	case TokenType::LEFT_PAREN: return "'('";
	case TokenType::RIGHT_PAREN: return "')'";
	case TokenType::VARIABLE: return "variable";
	case TokenType::FUNCTION: return "function";
	case TokenType::END_OF_SOURCE: return "end of source";
	case TokenType::ERROR:
		// Error tokens should be ignored.
		ASSERT_NOT_REACHED();
		return nullptr;
	case TokenType::WHITESPACE: 
		// Errors shouldn't happen on whitespace.
		ASSERT_NOT_REACHED();
		return nullptr;
	}
	ASSERT_NOT_REACHED();
	return nullptr;
};

void outputParserErrorMessage(std::ostream& out, const ParserError& error, std::string_view source, bool printLocation) {
	std::visit(overloaded{
		[&](const UnexpectedTokenParserError& e) {
			if (e.token.type == TokenType::ERROR) {
				return;
			}
			const auto& location = e.token.location;
			putnn(out, "unexpected %", tokenTypeName(e.token.type));
			if (printLocation) {
				out << '\n';
				highlightInText(out, source, location.start, location.length);
				out << '\n';
			}
		},
		[&](const ExpectedTokenParserError& e) {
			if (e.found.type == TokenType::ERROR) {
				return;
			}
			const auto& location = e.found.location;
			putnn(out, "expected % found %", tokenTypeName(e.expected), tokenTypeName(e.found.type));
			if (printLocation) {
				out << '\n';
				highlightInText(out, source, location.start, location.length);
				out << '\n';
			}
		},
	}, error);	
}
