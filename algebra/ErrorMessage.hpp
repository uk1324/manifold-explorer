#pragma once

#include "Parsing/ScannerMessageHandler.hpp"
#include "Parsing/ParserMessageHandler.hpp"
#include <ostream>

void outputScannerErrorMessage(std::ostream& out, const ScannerError& error, std::string_view source, bool printLocation);
void outputParserErrorMessage(std::ostream& out, const ParserError& error, std::string_view source, bool printLocation);