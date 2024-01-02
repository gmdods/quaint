#ifndef QUAINT_H
#define QUAINT_H

#include <cmath>
#include <cstdint>
#include <functional>
#include <ratio>
#include <type_traits>

namespace quaint {

namespace dim {

// Type sequence

// Andrei Alexandrescu : Embracing (and also Destroying) Variant Types Safely
// https://www.youtube.com/watch?v=va9I2qivBOA

template <typename... Ts>
struct seq {};

template <typename T, typename R>
struct cons;

template <typename T, typename... Ts>
struct cons<T, seq<Ts...>> {
	using type = seq<T, Ts...>;
};

template <typename T, typename R>
using cons_t = typename cons<T, R>::type;

// Exponentials

/* module Quaint where
 *
 * import Prelude hiding (exponent)
 *
 * data Exp = Exp {basis :: !String, exponent :: !Integer} deriving (Show, Eq)
 */

template <typename T, intmax_t E>
struct exp {
	using type = T;
};

template <typename T>
struct basis;

template <typename T, intmax_t E>
struct basis<exp<T, E>> {
	using type = T;
};

template <typename T>
using basis_t = typename basis<T>::type;

/*
 * instance Num Exp where
 *	negate e = e { exponent = negate (exponent e)}
 *	(+) e1 e2 = e1 { exponent = exponent e1 + exponent e2}
 *	(*) e1 e2 = e1 { exponent = exponent e1 * exponent e2} -- unused
 *	fromInteger e = Exp { basis = "", exponent = e } -- unused
 *	abs e = e { exponent = abs (exponent e)} -- unused
 *	signum e = e { exponent = signum (exponent e)} -- unused
 *
 */

template <typename E1, typename E2>
struct plus {
	using type = void;
};

template <typename T, intmax_t E1, intmax_t E2>
struct plus<exp<T, E1>, exp<T, E2>> {
	using type = exp<T, (E1 + E2)>;
};

template <typename E1, typename E2>
using plus_t = typename plus<E1, E2>::type;

template <typename E1, typename E2>
struct minus {
	using type = void;
};

template <typename T, intmax_t E1, intmax_t E2>
struct minus<exp<T, E1>, exp<T, E2>> {
	using type = exp<T, (E1 - E2)>;
};

template <typename E1, typename E2>
using minus_t = typename minus<E1, E2>::type;

template <typename E>
struct negate {
	using type = void;
};

template <typename T, intmax_t E>
struct negate<exp<T, E>> {
	using type = exp<T, -E>;
};

template <typename E>
using negate_t = typename negate<E>::type;

/*
 * add :: [Exp] -> [Exp] -> [Exp]
 * add (hd1 : tl1) (hd2 : tl2)
 *	| basis hd1 == basis hd2 = (hd1 + hd2) : add tl1 tl2
 *	| otherwise = hd1 : add tl1 (hd2 : tl2)
 * add [] (hd : tl) = hd : tl -- exclusive cases
 * add e [] = e
 */

template <typename D1, typename D2>
struct add;

template <typename D1_hd, typename D2_hd, typename... D1_tl, typename... D2_tl>
struct add<seq<D1_hd, D1_tl...>, seq<D2_hd, D2_tl...>> {
	using type = std::conditional_t<
	    std::is_same<basis_t<D1_hd>, basis_t<D2_hd>>::value,
	    cons_t<plus_t<D1_hd, D2_hd>,
		   typename add<seq<D1_tl...>, seq<D2_tl...>>::type>,
	    cons_t<D1_hd,
		   typename add<seq<D1_tl...>, seq<D2_hd, D2_tl...>>::type>>;
};

template <typename D_hd, typename... D_tl>
struct add<seq<>, seq<D_hd, D_tl...>> {
	using type = seq<D_hd, D_tl...>;
};

template <typename... D_seq>
struct add<seq<D_seq...>, seq<>> {
	using type = seq<D_seq...>;
};

template <typename D1, typename D2>
using add_t = typename add<D1, D2>::type;

/*
 * diff :: [Exp] -> [Exp] -> [Exp]
 * diff (hd1 : tl1) (hd2 : tl2)
 *	| basis hd1 == basis hd2 = (hd1 - hd2) : diff tl1 tl2
 *	| otherwise = hd1 : diff tl1 (hd2 : tl2)
 * diff [] (hd : tl) = -hd : diff [] tl
 * diff e [] = e
 */

template <typename D1, typename D2>
struct diff;

template <typename D1_hd, typename D2_hd, typename... D1_tl, typename... D2_tl>
struct diff<seq<D1_hd, D1_tl...>, seq<D2_hd, D2_tl...>> {
	using type = std::conditional_t<
	    std::is_same<basis_t<D1_hd>, basis_t<D2_hd>>::value,
	    cons_t<minus_t<D1_hd, D2_hd>,
		   typename diff<seq<D1_tl...>, seq<D2_tl...>>::type>,
	    cons_t<D1_hd,
		   typename diff<seq<D1_tl...>, seq<D2_hd, D2_tl...>>::type>>;
};

template <typename D_hd, typename... D_tl>
struct diff<seq<>, seq<D_hd, D_tl...>> {
	using type =
	    cons_t<negate_t<D_hd>, typename diff<seq<>, seq<D_tl...>>::type>;
};

template <typename... D_seq>
struct diff<seq<D_seq...>, seq<>> {
	using type = seq<D_seq...>;
};

template <typename D1, typename D2>
using diff_t = typename diff<D1, D2>::type;

/* expt :: [Exp] -> Integer -> [Exp]
 * expt e n
 *	| n == 0 = map (* Exp "" 0) e
 *	| n == (-1) = map negate e
 *	| n == 1 = e
 *	| otherwise =
 *		let recurse = expt e (div n 2)
 *		    square = add recurse recurse
 *		in if mod n 2 == 1
 *			then add square e
 *			else square
 */

template <typename D, intmax_t N>
struct expt;

template <typename... D_seq, intmax_t N>
struct expt<seq<D_seq...>, N> {
	using recurse = typename expt<seq<D_seq...>, (N / 2)>::type;
	using square = add_t<recurse, recurse>;
	using type = std::conditional_t<(N % 2 == 1),
					add_t<square, seq<D_seq...>>, square>;
};

template <typename D_hd, typename... D_tl>
struct expt<seq<D_hd, D_tl...>, -1> {
	using type =
	    cons_t<negate_t<D_hd>, typename expt<seq<D_tl...>, -1>::type>;
};

template <typename D_hd, typename... D_tl>
struct expt<seq<D_hd, D_tl...>, 0> {
	using type =
	    cons_t<exp<basis_t<D_hd>, 0>, typename expt<seq<D_tl...>, 0>::type>;
};

template <typename... D_seq>
struct expt<seq<D_seq...>, 1> {
	using type = seq<D_seq...>;
};

template <intmax_t N>
struct expt<seq<>, N> {
	using type = seq<>;
};

template <typename D, intmax_t N>
using expt_t = typename expt<D, N>::type;

} // namespace dim

// https://en.wikipedia.org/wiki/International_System_of_Quantities
struct q_length {};
struct q_mass {};
struct q_time {};

using Rep = std::size_t;

template <typename R>
struct storage {
	R val;
	constexpr storage(R val) : val{val} {}

	constexpr R get() const { return val; }
};

template <typename U>
struct linear {
	friend constexpr bool operator==(U lhs, U rhs) {
		return lhs.val == rhs.val;
	}
	friend constexpr U operator+(U lhs, U rhs) {
		return {lhs.val + rhs.val};
	}
	friend constexpr U operator-(U lhs, U rhs) {
		return {lhs.val - rhs.val};
	}
	friend constexpr U operator+(U qty) { return {+qty.val}; }
	friend constexpr U operator-(U qty) { return {-qty.val}; }
};

template <typename Ratio, intmax_t N>
struct ratio_exponential_s {
	using recurse = typename ratio_exponential_s<Ratio, (N / 2)>::type;
	using square = std::ratio_multiply<recurse, recurse>;
	using type =
	    std::conditional_t<(N % 2 == 1), std::ratio_multiply<square, Ratio>,
			       square>;
};

template <typename Ratio>
struct ratio_exponential_s<Ratio, 0> {
	using type = std::ratio<1, 1>;
};

template <typename Ratio>
struct ratio_exponential_s<Ratio, -1> {
	using type = std::ratio_divide<std::ratio<1, 1>, Ratio>;
};

template <typename Ratio>
struct ratio_exponential_s<Ratio, 1> {
	using type = Ratio;
};

template <typename Ratio, intmax_t N>
using ratio_exponential = typename ratio_exponential_s<Ratio, N>::type;

template <typename Ratio, typename Dim>
struct unit;

template <typename U1, typename U2>
using unit_multiply =
    unit<std::ratio_multiply<typename U1::ratio, typename U2::ratio>,
	 dim::add_t<typename U1::dim, typename U2::dim>>;

template <typename U1, typename U2>
using unit_divide =
    unit<std::ratio_divide<typename U1::ratio, typename U2::ratio>,
	 dim::diff_t<typename U1::dim, typename U2::dim>>;

template <typename U, intmax_t N>
using unit_expt = unit<ratio_exponential<typename U::ratio, N>,
		      dim::expt_t<typename U::dim, N>>;

template <typename Unit>
struct annotate {
	constexpr Unit operator()(unsigned val) const { return {val}; }

	template <typename Other>
	constexpr annotate<unit_multiply<Unit, Other>>
	operator*(annotate<Other>) const {
		return {};
	}

	template <typename Other>
	constexpr annotate<unit_divide<Unit, Other>>
	operator/(annotate<Other>) const {
		return {};
	}

	template <typename Other>
	constexpr bool operator==(annotate<Other>) const {
		return std::is_same<Unit, Other>::value;
	}
};

template <intmax_t Exp, typename Unit>
constexpr annotate<unit_expt<Unit, Exp>> expt(annotate<Unit>) {
	return {};
}

template <typename Ratio, typename Dim>
struct unit : storage<Rep>, linear<unit<Ratio, Dim>> {
	using ratio = Ratio;
	using dim = Dim;

	using Unit = unit<Ratio, Dim>;
	constexpr unit(Rep val) : storage(val) {}

	template <typename Other>
	constexpr unit_multiply<Unit, Other> operator*(Other o) const {
		return {val * o.val};
	}

	template <typename Other>
	constexpr unit_divide<Unit, Other> operator/(Other o) const {
		return {val / o.val};
	}

	template <typename Ratio_, typename Dim_>
	constexpr unit<Ratio_, Dim_> to(annotate<unit<Ratio_, Dim_>>) const {
		return {(val * Ratio_::den * Ratio::num) /
			(Ratio_::num * Ratio::den)};
	}
};

template <intmax_t Exp, typename Ratio, typename Dim>
constexpr unit_expt<unit<Ratio, Dim>, Exp> expt(unit<Ratio, Dim> u) {
	return {std::pow(u.val, Exp)};
}

using u_meters = unit<std::ratio<1, 1>, dim::seq<dim::exp<q_length, 1>>>;
using u_grams = unit<std::ratio<1, 1>, dim::seq<dim::exp<q_mass, 1>>>;
using u_inches = unit<std::ratio<127, 5'000>, dim::seq<dim::exp<q_length, 1>>>;
using u_seconds = unit<std::ratio<1, 1>, dim::seq<dim::exp<q_time, 1>>>;

template <typename Ratio, typename Dim>
constexpr annotate<unit<std::ratio_multiply<std::milli, Ratio>, Dim>>
milli(annotate<unit<Ratio, Dim>>) {
	return {};
}

template <typename Ratio, typename Dim>
constexpr annotate<unit<std::ratio_multiply<std::centi, Ratio>, Dim>>
centi(annotate<unit<Ratio, Dim>>) {
	return {};
}

template <typename Ratio, typename Dim>
constexpr annotate<unit<std::ratio_multiply<std::kilo, Ratio>, Dim>>
kilo(annotate<unit<Ratio, Dim>>) {
	return {};
}

constexpr auto meters = annotate<u_meters>{};
constexpr auto grams = annotate<u_grams>{};
constexpr auto seconds = annotate<u_seconds>{};
constexpr auto inches = annotate<u_inches>{};

} // namespace quaint

#endif // !QUAINT_H
