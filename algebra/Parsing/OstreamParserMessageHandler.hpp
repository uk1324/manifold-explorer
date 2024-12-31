#pragma once

#include "ParserMessageHandler.hpp"

struct OstreamParserMessageHandler : public ParserMessageHandler {
	OstreamParserMessageHandler(std::ostream& output);

	void initialize(std::string_view source) override;
	void onError(const ParserError& error) override;

	std::ostream& output;
	std::string_view source;
};