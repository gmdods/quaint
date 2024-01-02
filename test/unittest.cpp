#ifndef UNITTEST_MAIN
#include "unittest.h"
#include <assert.h>
#include <type_traits>

#include "../quaint.hpp"

#endif /* ifndef UNITTEST_MAIN */

using namespace quaint;

unittest("conversions") {
	auto measure = meters(3U);
	ensure(measure.get() == 3);
	ensure(measure.to(inches) == inches(118U));
	ensure(kilo(grams)(1U).to(grams) == grams(1'000U));
}

unittest("units") {
	auto measure = meters(3U);
	auto velocity = measure / seconds(1U);
	static_assert(
	    std::is_same<
		decltype(velocity),
		unit<std::ratio<1, 1>, dim::seq<dim::exp<q_length, 1>,
						dim::exp<q_time, -1>>>>::value,
	    "quantity division");

	static_assert(annotate<decltype(velocity)>{} == (meters / seconds),
		      "unit division");

	constexpr auto eg_newton_meter =
	    (kilo(grams) * (meters / expt<2>(seconds)) * meters);
	constexpr auto eg_joule =
	    annotate<decltype(kilo(grams)(1U) * expt<2>(velocity))>{};
	static_assert(eg_newton_meter == eg_joule, "unit multiply");
}
