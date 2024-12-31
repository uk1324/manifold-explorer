#pragma once

#include "ScannerMessageHandler.hpp"
#include <string_view>
#include <ostream>

struct OstreamScannerMessageHandler final : public ScannerMessageHandler {
	OstreamScannerMessageHandler(std::ostream& output);

	void initialize(std::string_view source) override;
	void onError(const ScannerError& error) override;

	std::ostream& output;
	std::string_view source;
};