#pragma once

#include <Types.hpp>
#include <variant>
#include "SourceInfo.hpp"

struct IllegalCharScannerError {
	u8 character;
	i64 sourceOffset;

	bool operator==(const IllegalCharScannerError&) const = default;
};

struct InvalidIdentifierScannerError {
	std::string_view identifier;
	SourceLocation location;

	bool operator==(const InvalidIdentifierScannerError&) const = default;
};

using ScannerError = std::variant<
	IllegalCharScannerError,
	InvalidIdentifierScannerError
>;

struct ScannerMessageHandler {
	virtual void initialize(std::string_view source) {};
	virtual void onError(const ScannerError& error) = 0;
};