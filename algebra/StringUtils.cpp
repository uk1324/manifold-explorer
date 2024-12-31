#include "stringUtils.hpp"
#include <Types.hpp>

bool isPrefix(std::string_view string, std::string_view possiblePrefix) {
	if (string.length() < possiblePrefix.length()) {
		return false;
	}

	for (usize i = 0; i < possiblePrefix.length(); i++) {
		if (string[i] != possiblePrefix[i]) {
			return false;
		}
	}

	return true;
}