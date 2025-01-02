#pragma once

#include "Ast.hpp"

bool astEquals(const Ast::Expr* a, const Ast::Expr* b, bool compareLocations);