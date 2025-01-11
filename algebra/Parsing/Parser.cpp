#include "Parser.hpp"
#include <Assertions.hpp>
#include <Put.hpp>
#include <charconv>
#include "CharUtils.hpp"

using namespace Ast;

Parser::Parser() {
	initialize(nullptr, std::string_view(), nullptr);
}

void Parser::initialize(
	const List<Token>* tokens,
	std::string_view source,
	ParserMessageHandler* messageHandler) {
	this->tokens = tokens;
	this->source = source;
	currentTokenIndex = 0;
	this->messageHandler = messageHandler;
	if (messageHandler != nullptr) {
		messageHandler->initialize(source);
	}
	astAllocator.reset();
}

std::optional<const Expr*> Parser::parse(
	const List<Token>& tokens,
	std::string_view source,
	ParserMessageHandler& messageHandler) {
	initialize(&tokens, source, &messageHandler);

	try {
		const auto root = expr();
		if (peek().type == TokenType::ERROR) {
			// Could try to synchronize.
			return std::nullopt;
			// Unexpected peek().type.
			//ASSERT(peek().type == TokenType::END_OF_FILE);
		} else if (peek().type != TokenType::END_OF_SOURCE) {
			messageHandler.onError(UnexpectedTokenParserError{ .token = peek() });
			return std::nullopt;
		}
		return root;
	}
	catch (const Error&) {
		return std::nullopt;
	}
}

Expr* Parser::expr() {
	return binaryExpr();
}

Expr* Parser::binaryExpr() {
	return plusOrMinusBinaryExpr();
}

Expr* Parser::plusOrMinusBinaryExpr() {
	const auto start = peek().start();
	auto lhs = timesOrDivideBinaryExpr();

	while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
		const auto operatorTokenType = peekPrevious().type;
		const auto rhs = timesOrDivideBinaryExpr();
		const auto end = peek().end();
		switch (operatorTokenType) {
		case TokenType::PLUS:
			lhs = astAllocator.allocate<BinaryExpr>(lhs, rhs, BinaryOpType::ADD, start, end);
			break;

		case TokenType::MINUS:
			lhs = astAllocator.allocate<BinaryExpr>(lhs, rhs, BinaryOpType::SUBTRACT, start, end);
			break;

		default:
			ASSERT_NOT_REACHED();
		}
	}

	return lhs;
}

Expr* Parser::timesOrDivideBinaryExpr() {
	const auto start = peek().start();
	auto lhsResult = primaryExpr();
	ASSERT(!lhsResult.absorbedLhs);
	auto lhs = lhsResult.expr;

	while (match(TokenType::STAR) || match(TokenType::SLASH)) {
		const auto operatorTokenType = peekPrevious().type;

		BinaryOpType op = BinaryOpType::MULTIPLY;
		switch (operatorTokenType) {
		case TokenType::STAR: op = BinaryOpType::MULTIPLY; break;
		case TokenType::SLASH: op = BinaryOpType::DIVIDE; break;
		default:
			ASSERT_NOT_REACHED();
			break;
		}

		const auto rhs = primaryExpr(LhsOfBinaryExpr{ .start = start, .lhs = lhs, .op = op });
		if (rhs.absorbedLhs) {
			lhs = rhs.expr;
			continue;
		}
		const auto end = peek().end();
		switch (operatorTokenType) {
		case TokenType::STAR:
			lhs = astAllocator.allocate<BinaryExpr>(lhs, rhs.expr, BinaryOpType::MULTIPLY, start, end);
			break;

		case TokenType::SLASH:
			lhs = astAllocator.allocate<BinaryExpr>(lhs, rhs.expr, BinaryOpType::DIVIDE, start, end);
			break;

		default:
			ASSERT_NOT_REACHED();
		}
	}

	return lhs;
}

Parser::PrimaryExprResult Parser::primaryExpr(std::optional<LhsOfBinaryExpr> binaryExprLhs) {
	Expr* lhs = nullptr;

	auto parenExprAfterMatch = [this]() -> Expr* {
		auto e = expr();
		expect(TokenType::RIGHT_PAREN);
		return e;
	};

	i64 lhsStart;

	bool absorbedLhs = false;
	if (match(TokenType::FLOAT)) {
		const auto& numberToken = peekPrevious();
		lhs = number();
		lhsStart = numberToken.start();

		if (match(TokenType::CARET)) {
			return Parser::PrimaryExprResult{
				exponentiationExpr(lhs, lhsStart),
				absorbedLhs
			};
		}

	} else if (match(TokenType::LEFT_PAREN)) {
		lhsStart = peek().start();
		lhs = parenExprAfterMatch();
		if (match(TokenType::CARET)) {
			return Parser::PrimaryExprResult{
				exponentiationExpr(lhs, lhsStart),
				absorbedLhs
			};
		}
	} else if (match(TokenType::VARIABLE)) {
		const auto& identifierToken = peekPrevious();
		lhsStart = identifierToken.start();
		const auto identifier = tokenSource(identifierToken);
		lhs = astAllocator.allocate<IdentifierExpr>(
			identifier,
			lhsStart,
			identifierToken.end());

		if (match(TokenType::CARET)) {
			return Parser::PrimaryExprResult{
				exponentiationExpr(lhs, lhsStart),
				absorbedLhs
			};
		}

	} else if (match(TokenType::FUNCTION)) {
		lhsStart = peekPrevious().start();
		lhs = function(tokenSource(peekPrevious()), lhsStart);

		if (match(TokenType::CARET)) {
			return Parser::PrimaryExprResult{
				exponentiationExpr(lhs, lhsStart),
				absorbedLhs
			};
		}
	} else if (match(TokenType::MINUS)) {
		// TODO: Not sure if this should be changed but -4x will parse to -(4 * x) and not (-4) * x. (I wrote this when I thought that the order is reversed but I guess -(4 * x) makes more sense thatn the other option. Don't think that will change the result but not sure. GCC treats the differently. I guess if x is NaN then the result might have different signs idk.
		const auto start = peekPrevious().start();
		const auto& operand = primaryExpr();
		ASSERT(!operand.absorbedLhs);
		const auto end = peek().end();
		const auto resultExpr = astAllocator.allocate<UnaryExpr>(
			operand.expr,
			UnaryOpType::NEGATE,
			start,
			end
		);
		return PrimaryExprResult{
			resultExpr,
			absorbedLhs
		};
	} else {
		auto nextNonWhitespaceToken = [this]() {
			while (peek().type == TokenType::WHITESPACE) {
				advance();
			}
			return peek();
		};
		throwError(UnexpectedTokenParserError{ .token = nextNonWhitespaceToken() });
	}

	for (;;) {
		i64 rhsStart;
		i64 rhsEnd;

		Expr* rhs;
		if (binaryExprLhs.has_value() && peek().type == TokenType::WHITESPACE) {
			const auto end = peekPrevious().end();

			// Should binaryExprRhs be set to nullptr
			// What if there are 2 whitespaces

			lhs = astAllocator.allocate<BinaryExpr>(binaryExprLhs->lhs, lhs, binaryExprLhs->op, binaryExprLhs->start, end);
			// Advancing here to not include whitespace in the expression.
			advance();
			absorbedLhs = true;
			// Doing this to handle things like a/b c d = ((a/b) * c) * d

			// a/a^b c d = 
			binaryExprLhs = std::nullopt;
			continue;
		} else if (match(TokenType::VARIABLE)) {
			const auto& identifierToken = peekPrevious();
			const auto identifier = tokenSource(identifierToken);
			const auto start = identifierToken.start();
			rhsStart = start;
			rhs = astAllocator.allocate<IdentifierExpr>(
				identifier,
				start,
				identifierToken.end());
			rhsEnd = identifierToken.end();
		} else if (match(TokenType::FLOAT)) {
			const auto& numberToken = peekPrevious();
			rhsStart = numberToken.start();
			rhsEnd = numberToken.end();
			rhs = number();
		} else if (match(TokenType::FUNCTION)) {
			rhsStart = peekPrevious().start();
			rhs = function(tokenSource(peekPrevious()), rhsStart);
			rhsEnd = peekPrevious().end();
		} else if (match(TokenType::LEFT_PAREN)) {
			rhsStart = peekPrevious().start();
			rhs = parenExprAfterMatch();
			rhsEnd = peekPrevious().end();
		} else {
			break;
		}

		if (match(TokenType::CARET)) {
			rhs = exponentiationExpr(rhs, rhsStart);
			return PrimaryExprResult{
				astAllocator.allocate<BinaryExpr>(lhs, rhs, BinaryOpType::MULTIPLY, lhsStart, rhsEnd),
				absorbedLhs
			};
		}

		lhs = astAllocator.allocate<BinaryExpr>(lhs, rhs, BinaryOpType::MULTIPLY, lhsStart, rhsEnd);
	}
	
	return PrimaryExprResult{
		lhs,
		absorbedLhs
	};
}

Expr* Parser::exponentiationExpr(Expr* lhs, i64 start) {
	const auto rhs = primaryExpr(LhsOfBinaryExpr{ .start = start, .lhs = lhs, .op = BinaryOpType::EXPONENTIATE });
	if (rhs.absorbedLhs) {
		return rhs.expr;
	}
	const auto end = peekPrevious().end();
	return astAllocator.allocate<BinaryExpr>(lhs, rhs.expr , BinaryOpType::EXPONENTIATE, start, end);
}


Expr* Parser::function(std::string_view name, i64 start) {
	// TODO: Maybe make a new error expected left paren after function name.
	expect(TokenType::LEFT_PAREN);

	AstAllocator::List<const Expr*> arguments;

	if (!eof() && peek().type != TokenType::RIGHT_PAREN) {
		do {
			astAllocator.listAppend(arguments, (const Expr*)(expr()));
		} while (!eof() && match(TokenType::COMMA));
	}
	expect(TokenType::RIGHT_PAREN);

	return astAllocator.allocate<FunctionExpr>(name, arguments.span(), start, peek().end());
}

Expr* Parser::number() {
	const auto& numberToken = peekPrevious();
	const auto numberTokenSource = tokenSource(numberToken);

	ASSERT(numberTokenSource.length() > 0);
	IntType integerPart = 0;
	IntType fractionalPartNumerator = 0;
	IntType fractionalPartDenominator = 1;
	bool parsingIntegerPart = true;
	for (auto& c : numberTokenSource) {
		if (isDigit(c)) {
			const auto digit = c - '0';
			if (parsingIntegerPart) {
				integerPart *= 10;
				integerPart += digit;
			} else {
				fractionalPartNumerator += digit;
				fractionalPartDenominator *= 10;
			}
		} else if (c == '.') {
			parsingIntegerPart = false;
		} else {
			ASSERT_NOT_REACHED();
			return astAllocator.allocate<ConstantExpr>(1, 1, numberToken.start(), numberToken.end());
		}
	}

	return astAllocator.allocate<ConstantExpr>(
		integerPart * fractionalPartDenominator + fractionalPartNumerator,
		fractionalPartDenominator,
		numberToken.start(),
		numberToken.end());
}

const Token& Parser::peek() {
	ASSERT(currentTokenIndex >= 0);
	return (*tokens)[static_cast<usize>(currentTokenIndex)];
}

const Token& Parser::peekPrevious() {
	ASSERT(currentTokenIndex > 0);
	return (*tokens)[static_cast<usize>(currentTokenIndex) - 1];
}

bool Parser::check(TokenType type) {
	return peek().type == type;
}

bool Parser::match(TokenType type, bool ignoreWhitespace) {
	std::optional<i64> posBeforeSkipping;
	if (ignoreWhitespace) {
		posBeforeSkipping = currentTokenIndex;
		while (peek().type == TokenType::WHITESPACE) {
			advance();
		}
	}

	if (peek().type == type) {
		advance();
		return true;
	} 
	if (posBeforeSkipping.has_value()) {
		currentTokenIndex = *posBeforeSkipping;
	}
	return false;
}

void Parser::expect(TokenType type) {
	if (match(type)) {
		return;
	}
	throwError(ExpectedTokenParserError{ .expected = type, .found = peek() });
}

void Parser::advance() {
	ASSERT(currentTokenIndex >= 0);
	//put("%", tokenTypeToStr((*tokens)[currentTokenIndex].type));
	if ((*tokens)[static_cast<usize>(currentTokenIndex)].type == TokenType::END_OF_SOURCE) {
		return;
	}
	currentTokenIndex++;
}

bool Parser::eof() {
	return peek().type == TokenType::END_OF_SOURCE;
}

void Parser::throwError(const ParserError& error) {
	while (peek().type == TokenType::WHITESPACE) {
		advance();
	}
	messageHandler->onError(error);
	throw Error();
}

std::string_view Parser::tokenSource(const Token& token) const {
	return source.substr(token.location.start, token.location.length);
}