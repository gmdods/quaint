#ifndef QUAINT_H
#define QUAINT_H

#include <ratio>

namespace quaint {

// https://en.wikipedia.org/wiki/International_System_of_Quantities
template <intmax_t Length, intmax_t Mass, intmax_t Time>
struct isq {
	static constexpr intmax_t length = Length;
	static constexpr intmax_t mass = Mass;
	static constexpr intmax_t time = Time;
};

template <typename isq1, typename isq2>
using isq_add = isq<isq1::length + isq2::length, isq1::mass + isq2::mass,
		    isq1::time + isq2::time>;

template <typename isq1, typename isq2>
using isq_sub = isq<isq1::length - isq2::length, isq1::mass - isq2::mass,
		    isq1::time - isq2::time>;

using q_length = isq<1, 0, 0>;
using q_mass = isq<0, 1, 0>;
using q_time = isq<0, 0, 1>;

using Rep = std::size_t;

template <typename R>
struct storage {
	R val;
	storage(R val) : val{val} {}

	R get() const { return val; }
};

template <typename U>
struct linear {
	friend bool operator==(U lhs, U rhs) { return lhs.val == rhs.val; }
	friend U operator+(U lhs, U rhs) { return {lhs.val + rhs.val}; }
	friend U operator-(U lhs, U rhs) { return {lhs.val - rhs.val}; }
	friend U operator+(U qty) { return {+qty.val}; }
	friend U operator-(U qty) { return {-qty.val}; }
};

template <typename Ratio, typename Dim>
struct unit;

template <typename Unit1, typename Unit2>
using unit_multiply =
    unit<std::ratio_multiply<typename Unit1::ratio, typename Unit2::ratio>,
	 isq_add<typename Unit1::dim, typename Unit2::dim>>;

template <typename Unit1, typename Unit2>
using unit_divide =
    unit<std::ratio_divide<typename Unit1::ratio, typename Unit2::ratio>,
	 isq_sub<typename Unit1::dim, typename Unit2::dim>>;

template <typename Unit>
struct annotate {
	Unit operator()(unsigned val) const { return {val}; }

	template <typename Other>
	annotate<unit_multiply<Unit, Other>> operator*(annotate<Other>) const {
		return {};
	}

	template <typename Other>
	annotate<unit_divide<Unit, Other>> operator/(annotate<Other>) const {
		return {};
	}
};

template <typename Ratio, typename Dim>
struct unit : storage<Rep>, linear<unit<Ratio, Dim>> {
	using ratio = Ratio;
	using dim = Dim;

	using Unit = unit<Ratio, Dim>;
	unit(Rep val) : storage(val) {}

	template <typename Other>
	unit_multiply<Unit, Other> operator*(Other o) const {
		return {val * o.val};
	}

	template <typename Other>
	unit_divide<Unit, Other> operator/(Other o) const {
		return {val / o.val};
	}

	template <typename Ratio_, typename Dim_>
	unit<Ratio_, Dim_> to(annotate<unit<Ratio_, Dim_>>) const {
		return {(val * Ratio_::den * Ratio::num) /
			(Ratio_::num * Ratio::den)};
	}
};

using u_meters = unit<std::ratio<1, 1>, q_length>;
using u_inches = unit<std::ratio<254, 10'000>, q_length>;
using u_seconds = unit<std::ratio<1, 1>, q_time>;

constexpr auto meters = annotate<u_meters>{};
constexpr auto seconds = annotate<u_seconds>{};
constexpr auto inches = annotate<u_inches>{};

} // namespace quaint

#endif // !QUAINT_H
