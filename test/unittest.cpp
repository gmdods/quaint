#ifndef UNITTEST_MAIN
#include "unittest.h"
#include <assert.h>
#include <type_traits>

#include "../quaint.hpp"

#endif /* ifndef UNITTEST_MAIN */

unittest("conversions") {
	using namespace quaint;
	auto measure = meters(3U);
	ensure(measure.get() == 3);
	ensure(measure.to(inches) == inches(118U));

	constexpr auto same_units =
	    std::is_same<decltype(measure / seconds(1U)),
			 decltype((meters / seconds)(3U))>::value;
	static_assert(same_units, "ERROR");
}
