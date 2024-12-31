add_executable(tests "tests.cpp" "TestingScannerMessageHandler.cpp" "TestRunner.cpp" "TestingParserMessageHandler.cpp" "Utils.cpp")
target_compile_features(tests PUBLIC cxx_std_23)
target_link_libraries(tests algebra)
target_include_directories(tests PRIVATE "../")