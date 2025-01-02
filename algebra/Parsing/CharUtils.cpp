#include "CharUtils.hpp"

bool isDigit(u8 c) {
	return c >= '0' && c <= '9';
}

bool isAlpha(u8 c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}