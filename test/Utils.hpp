#pragma once

#include <algebra/Parsing/Token.hpp>
#include <View.hpp>

void printToken(std::string_view source, const Token& token);
void printTokens(std::string_view source, const View<const Token> tokens);