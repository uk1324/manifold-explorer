#pragma once

// ASSERT - Crash.
// CHECK - Just for debug checks. Should not happen, but if it does it is handled.

#if defined(__GNUC__) || defined(__GNUG__) 
#define FUNCTION_SIGNATURE __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
#define FUNCTION_SIGNATURE __FUNCSIG__
#endif

// Returns condition back.
bool assertImplementation(bool condition, const char* functionName, int line);
[[noreturn]] void assertNotReachedImplementation(const char* functionName, int line);

#define ASSERT(condition) assertImplementation(condition, FUNCTION_SIGNATURE, __LINE__)
#define ASSERT_NOT_REACHED() assertNotReachedImplementation(FUNCTION_SIGNATURE, __LINE__);

#ifdef DEBUG
#define DEBUG_ASSERT(condition) assertImplementation(condition, FUNCTION_SIGNATURE, __LINE__)
#define DEBUG_ASSERT_NOT_REACHED() ASSERT(false)
#else 
#define DEBUG_ASSERT(condition)
#define DEBUG_ASSERT_NOT_REACHED()
#endif

// TODO: For checks maybe allow continuing
#ifdef FINAL_RELEASE
#define CHECK(condition)
#else
#define CHECK(condition) assertImplementation(condition, FUNCTION_SIGNATURE, __LINE__)
#endif

#define CHECK_NOT_REACHED() CHECK(false)