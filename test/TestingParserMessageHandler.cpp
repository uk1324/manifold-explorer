#include "TestingParserMessageHandler.hpp"

TestingParserMessageReporter::TestingParserMessageReporter(std::ostream& ostream)
	: messageHandler(ostream) {
}

void TestingParserMessageReporter::initialize(std::string_view source) {
	messageHandler.initialize(source);
	errors.clear();
}

void TestingParserMessageReporter::onError(const ParserError& error) {
	messageHandler.onError(error);
	errors.push_back(error);
}
