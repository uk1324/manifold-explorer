#include "TestingScannerMessageHandler.hpp"

TestingScannerMessageHandler::TestingScannerMessageHandler(std::ostream& output)
	: messageHandler(output) {
}

void TestingScannerMessageHandler::initialize(std::string_view source) {
	messageHandler.initialize(source);
	errors.clear();
}

void TestingScannerMessageHandler::onError(const ScannerError& error) {
	messageHandler.onError(error);
	errors.push_back(error);
}