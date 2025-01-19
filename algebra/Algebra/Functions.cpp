#include "Functions.hpp"
#include "ConstructionHelpers.hpp";

using namespace AlgebraConstuctionHelpers;

Sin::Sin()
	: Function(FunctionType::SIN, "sin", 1) {}

Cos::Cos()
	: Function(FunctionType::COS, "cos", 1) {}

Ln::Ln()
	: Function(FunctionType::LN, "ln", 1) {}

Abs::Abs()
	: Function(FunctionType::ABS, "abs", 1) {}

Tan::Tan()
	: Function(FunctionType::TAN, "tan", 1) {}

Asin::Asin()
	: Function(FunctionType::ASIN, "asin", 1) {}

Acos::Acos()
	: Function(FunctionType::ACOS, "acos", 1) {}

Atan::Atan()
	: Function(FunctionType::ATAN, "atan", 1) {}