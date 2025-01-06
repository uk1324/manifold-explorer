#include "Functions.hpp"
#include "ConstructionHelpers.hpp";

using namespace AlgebraConstuctionHelpers;

Sin::Sin()
	: Function(FunctionType::SIN, "sin", 1) {}

Cos::Cos()
	: Function(FunctionType::COS, "cos", 1) {}

Ln::Ln()
	: Function(FunctionType::LN, "ln", 1) {}