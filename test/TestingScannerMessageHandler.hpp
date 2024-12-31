#pragma once

#include <algebra/Parsing/OstreamScannerMessageHandler.hpp>
#include <vector>

struct TestingScannerMessageHandler : public ScannerMessageHandler {
	TestingScannerMessageHandler(std::ostream& output);

	void initialize(std::string_view source) override;
	void onError(const ScannerError& error) override;

	std::vector<ScannerError> errors;

	OstreamScannerMessageHandler messageHandler;
};