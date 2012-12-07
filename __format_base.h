/*-
 * Copyright (c) 2012, 2013 Zhihao Yuan.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef ___FORMAT_BASE_H
#define ___FORMAT_BASE_H 1

#include <cstdlib>
#include <stdexcept>
#include <tuple>

namespace stdex {

template <typename T>
using identity = std::common_type<T>;

template <typename T, typename CharT>
struct _accept_narrow : std::false_type {};

template <>
struct _accept_narrow<char, char> : std::false_type {};

template <typename T>
struct _accept_narrow<T, char> : std::true_type {
	typedef char		char_type;
	typedef int		int_type;
};

template <>
struct _accept_narrow<char, signed char> : std::true_type {
	typedef signed char	char_type;
	typedef int		int_type;
};

template <>
struct _accept_narrow<char, unsigned char> : std::true_type {
	typedef unsigned char	char_type;
	typedef int		int_type;
};

template <typename Iter, typename... T>
struct _format {
	_format(Iter const& it1, Iter const& it2, T const&... t) :
		iter_(it1, it2), item_(t...) {}

	Iter& begin() {
		return iter_.first;
	}

	Iter& end() {
		return iter_.second;
	}

	template <size_t _I, typename _Iter, typename... _T>
	friend auto get(_format<_Iter, _T...> const&)
		-> typename std::tuple_element<_I,
		std::tuple<_T const&...>>::type;

private:
	std::pair<Iter, Iter>	iter_;
	std::tuple<T const&...>	item_;
};

template <size_t I, typename Iter, typename... T>
auto get(_format<Iter, T...> const& o)
	-> typename std::tuple_element<I, std::tuple<T const&...>>::type {
	return std::get<I>(o.item_);
}

template <typename Stream>
struct _unformatted_guard {
	_unformatted_guard(Stream& s) :
		stream_(s), flags_(s.flags()), width_(s.width()) {}
	~_unformatted_guard() {
		stream_.width(width_);
		stream_.flags(flags_);
	}

	_unformatted_guard(_unformatted_guard const&) = delete;
	_unformatted_guard& operator=(_unformatted_guard const&) = delete;

private:
	Stream&				stream_;
	decltype(stream_.flags())	flags_;
	decltype(stream_.width())	width_;
};

template <int Base, typename Int>
int _lexical_width(Int i) {
	if (i == 0)
		return 1;
	int n = 0;
	while (i != 0) {
		i /= Base;
		++n;
	}
	return n;
}

}

#endif
