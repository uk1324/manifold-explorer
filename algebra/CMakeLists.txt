add_library(algebra STATIC "Utils/Assertions.cpp"  "Utils/Put.cpp" "Algebra/AlgebraicExpr.natvis"
"Parsing/Token.cpp" "Parsing/Scanner.cpp" "StringUtils.cpp" "Parsing/SourceInfo.cpp" "Parsing/OstreamScannerMessageHandler.cpp" "ErrorMessage.cpp" "PrintingUtils.cpp" "Parsing/Parser.cpp" "Parsing/Ast.cpp" "Parsing/AstAllocator.cpp"  "Parsing/OstreamParserMessageHandler.cpp" "Parsing/AstEquals.cpp" "Parsing/AstPrint.cpp" "Algebra/Expr.cpp" "Algebra/AstToExpr.cpp" "Parsing/CharUtils.cpp" "Algebra/PrintExpr.cpp" "Algebra/Simplification.cpp" "Algebra/Derivative.cpp" "Algebra/ConstructionHelpers.cpp" "Algebra/Functions.cpp" "Algebra/Context.cpp" "Algebra/Integral.cpp" "Algebra/OdeSolve.cpp" "Algebra/Polynomial.cpp")
target_compile_features(algebra PUBLIC cxx_std_23)
# enumerator 'identifier' in switch of enum 'enumeration' is not handled
target_compile_options(algebra PRIVATE /we4062)
target_include_directories(algebra PUBLIC "./Utils")