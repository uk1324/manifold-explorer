#pragma once

#include <algebra/Parsing/OstreamParserMessageHandler.hpp>
#include <vector>

struct TestingParserMessageReporter : public ParserMessageHandler {
	TestingParserMessageReporter(std::ostream& ostream);

	void initialize(std::string_view source) override;
	void onError(const ParserError& error) override;

	std::vector<ParserError> errors;

	OstreamParserMessageHandler messageHandler;
};