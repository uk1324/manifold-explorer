#include "RandomExprGenerator.hpp"
#include "Assertions.hpp"

#define CHECK_NESTING() \
	if (nesting <= 0) { \
		out << '('; \
		if (randomBool()) { \
			symbolExpr(); \
		} else { \
			constantExpr(); \
		} \
		out << ')'; \
		return; \
	}

RandomInputGenerator::RandomInputGenerator()
	: rng(dev()) 
	, symbols(decltype(symbols)::empty())
	, functions(decltype(functions)::empty()) {
}

std::string_view RandomInputGenerator::generate(
	View<const Algebra::Symbol* const> symbols,
	View<const Algebra::Function* const> functions) {
	this->symbols = symbols;
	this->functions = functions;
	out.string().clear();
	const auto nesting = randomFrom0To(maxNestingDepth);
	expr(nesting);
	return out.string();
}

void RandomInputGenerator::expr(i32 nesting) {
	CHECK_NESTING();

	enum Type {
		SYMBOL,
		SUM,
		PRODUCT,
		FUNCTION,
		POWER,
		DIVISION,
		SUBTRACTION,
		NEGATION,
		CONSTANT,

		COUNT,
	} const type = Type(randomFrom0To(COUNT));

	whitespace();
	switch (type) {
	case SYMBOL: {
		symbolExpr();
		return;
	}
	case SUM: {
		nAryExpr(nesting - 1, "+");
		return;
	}
	case PRODUCT: {
		const auto implicitProduct = randomBool();
		if (implicitProduct) {
			nAryExpr(nesting - 1, "*");
		} else {
			nAryExpr(nesting - 1, "", false);
		}
		return;
	}
	case FUNCTION: {
		auto& function = functions[randomIndex(functions.size())];
		out << function->name;
		whitespace();
		out << '(';
		whitespace();
		for (i64 i = 0; i < function->arity - 1; i++) {
			expr(nesting - 1);
			whitespace();
			out << ',';
			whitespace();
		}
		if (function->arity > 0) {
			expr(nesting - 1);
			whitespace();
		}
		out << ')';
		whitespace();
		return;
	}
	case POWER: {
		expr(nesting - 1);
		whitespace();
		out << "^";
		whitespace();
		expr(nesting - 1);
		return;
	}
	case DIVISION: {
		nAryExpr(nesting - 1, "/");
		return;
	}
	case SUBTRACTION: {
		nAryExpr(nesting - 1, "-");
		return;
	}
	case NEGATION: {
		const char op[] = "-";
		out << op[randomIndex(std::size(op) - 1)];
		whitespace();
		expr(nesting - 1);
		return;
	}
	case CONSTANT: {
		constantExpr();
		return;
	}

	}
	whitespace();
	ASSERT_NOT_REACHED();
}

void RandomInputGenerator::nAryExpr(i32 nesting, const char* op, bool addWhitespace) {
	const bool useParens = randomBool();

	if (useParens) {
		out << '(';
		whitespace();
	}

	const auto arity = randomInRangeExclusive(2, maxArity);
	for (i32 i = 0; i < arity; i++) {
		expr(nesting - 1);
		if (i != arity - 1) {
			if (addWhitespace) {
				whitespace();
			}
			out << op;
			if (addWhitespace) {
				whitespace();
			}
		}
	}

	if (useParens) {
		out << ')';
	}
}

void RandomInputGenerator::constantExpr() {
	const auto integerPartLength = randomInRangeInclusive(1, integerPartMaxLength);
	ASSERT(integerPartLength > 0);

	for (i32 i = 0; i < integerPartLength; i++) {
		out << randomDigit();
	}

	bool fractionalPart = randomBool();
	if (!fractionalPart) {
		return;
	}
	out << ".";

	const auto fractionalPartLength = randomFrom0To(fractionalPartMaxLength);
	for (i32 i = 1; i < fractionalPartLength; i++) {
		out << randomDigit();
	}
}

void RandomInputGenerator::symbolExpr() {
	const auto name = symbols[randomIndex(symbols.size())]->name;
	out << name;
	if (isdigit(name.back())) {
		out << ' ';
	}
}

void RandomInputGenerator::whitespace() {
	const auto length = randomFrom0To(whitespaceMaxLength);
	for (i32 i = 0; i < length; i++) {
		out << ' ';
	}
}

//i32 RandomInputGenerator::randomNumber() {
//	return rand();
//}

bool RandomInputGenerator::randomBool() {
	return std::uniform_int_distribution<int>(0, 1)(rng) == 0;
}

i32 RandomInputGenerator::randomFrom0To(i32 x) {
	return std::uniform_int_distribution<i32>(0, x - 1)(rng);
	//return randomNumber() % x;
}

usize RandomInputGenerator::randomIndex(usize size) {
	if (size == 0) {
		ASSERT_NOT_REACHED();
		return 0;
	}
	return std::uniform_int_distribution<usize>(0, size - 1)(rng);
}

i32 RandomInputGenerator::randomInRangeExclusive(i32 includingStart, i32 excludingEnd) {
	return std::uniform_int_distribution<i32>(includingStart, excludingEnd - 1)(rng);
}

i32 RandomInputGenerator::randomInRangeInclusive(i32 includingStart, i32 includingEnd) {
	return std::uniform_int_distribution<i32>(includingStart, includingEnd)(rng);
}

char RandomInputGenerator::randomDigit() {
	//char digits[] = "1234567890";
	char digits[] = "123";
	// -1 because of the null byte.
	return digits[randomIndex(std::size(digits) - 1)];
}