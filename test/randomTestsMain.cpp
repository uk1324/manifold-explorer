#include "randomTestsMain.hpp"
#include "TestRunner.hpp"
#include <Put.hpp>
#include "RandomExprGenerator.hpp"
#include <algebra/Algebra/Simplification.hpp>

void randomTestsMain() {
	TestRunner t;
	t.context.addVariable("x");
	t.context.addVariable("y");
	t.context.addVariable("z");

	RandomInputGenerator g;
	g.rng.seed(21);
	for (i32 i = 0; i < 1000000; i++) {
		const auto source = g.generate(constView(t.context.symbols), constView(t.context.functions));
		const auto expr = t.tryCompileSourceToAlgebraicExpr("random", source);
		if (!expr.has_value()) {
			continue;
		}
		const auto simplified = Algebra::basicSimplifiy(t.context, *expr);
		if (!Algebra::isSimplifiedExpr(t.context, simplified)) {
			put("%", source);
			t.printExpr(simplified);
			ASSERT_NOT_REACHED();
		}
	}
}
