#include "sourceInfo.hpp"
#include <Assertions.hpp>

i64 SourceLocation::end() const {
	return start + length;
}

SourceLocation SourceLocation::fromStartEnd(i64 start, i64 end) {
	ASSERT(start >= 0);
	ASSERT(end >= 0);
	ASSERT(start <= end); // start = end can happen for example for END_OF_SOURCE tokens.
	return SourceLocation{
		.start = start,
		.length = end - start
	};
}

SourceLocation SourceLocation::fromStartLength(i64 start, i64 length) {
	ASSERT(start >= 0);
	ASSERT(length >= 0);
	return SourceLocation{
		.start = start,
		.length = length
	};
}