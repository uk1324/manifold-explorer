add_executable(tests "tests.cpp" "TestingScannerMessageHandler.cpp" "TestRunner.cpp" "TestingParserMessageHandler.cpp" "Utils.cpp" "RandomExprGenerator.cpp" "randomTestsMain.cpp" "../engine/engine/Utils/StringStream.cpp")
target_compile_features(tests PUBLIC cxx_std_23)
target_link_libraries(tests algebra)
target_include_directories(tests PRIVATE "../")