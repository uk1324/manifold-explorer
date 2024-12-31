#include "OstreamParserMessageHandler.hpp"
#include "../ErrorMessage.hpp"

OstreamParserMessageHandler::OstreamParserMessageHandler(std::ostream& output)
	: output(output) {}

void OstreamParserMessageHandler::initialize(std::string_view source) {
	this->source = source;
}

void OstreamParserMessageHandler::onError(const ParserError& error) {
	outputParserErrorMessage(output, error, source, true);
}