#include "OstreamScannerMessageHandler.hpp"
#include "../ErrorMessage.hpp"

OstreamScannerMessageHandler::OstreamScannerMessageHandler(std::ostream& output)
	: output(output) {}

void OstreamScannerMessageHandler::initialize(std::string_view source) {
	this->source = source;
}

void OstreamScannerMessageHandler::onError(const ScannerError& error) {
	outputScannerErrorMessage(output, error, source, true);
}