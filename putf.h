/*-
 * Copyright (c) 2012, 2013, 2015 Zhihao Yuan.  All rights reserved.
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

#ifndef _PUTF_H
#define _PUTF_H 1

#include "__format_base.h"
#include <algorithm>
#include <cmath>
#include <iosfwd>
#include <sstream>

namespace stdex {

template <typename CharT, typename... T>
inline auto putf(CharT const *fmt, T const&... t)
	-> _fmt_put<CharT, T...> {
	return { fmt, fmt + std::char_traits<CharT>::length(fmt), t... };
}

template <typename CharT, typename Traits, typename Allocator, typename... T>
inline auto putf(std::basic_string<CharT, Traits, Allocator> const& fmt,
    T const&... t)
	-> _fmt_put<CharT, T...> {
	return { fmt.data(), fmt.data() + fmt.size(), t... };
}

template <typename CharT, typename Tuple, size_t... I>
inline auto _vputf(CharT const *fmt, Tuple const& t, _indices<I...>)
	-> _fmt_put<CharT,
	typename std::remove_const<
	typename std::remove_reference<decltype(get<I>(t))>::type>::type...> {
	return { fmt, fmt + std::char_traits<CharT>::length(fmt),
		get<I>(t)... };
}

template <typename CharT, typename Traits, typename Allocator, typename Tuple,
	size_t... I>
inline auto _vputf(std::basic_string<CharT, Traits, Allocator> const& fmt,
    Tuple const& t, _indices<I...>)
	-> _fmt_put<CharT,
	typename std::remove_const<
	typename std::remove_reference<decltype(get<I>(t))>::type>::type...> {
	return { fmt.data(), fmt.data() + fmt.size(), get<I>(t)... };
}

template <typename CharT, typename Tuple>
inline auto vputf(CharT const *fmt, Tuple const& t)
	-> decltype(_vputf(fmt, t, _tuple_indices<Tuple>())) {
	return _vputf(fmt, t, _tuple_indices<Tuple>());
}

template <typename CharT, typename Traits, typename Allocator, typename Tuple>
inline auto vputf(std::basic_string<CharT, Traits, Allocator> const& fmt,
    Tuple const& t)
	-> decltype(_vputf(fmt, t, _tuple_indices<Tuple>())) {
	return _vputf(fmt, t, _tuple_indices<Tuple>());
}

template <typename T, typename Enable = void>
struct _make_unsigned_fallback;

template <>
struct _make_unsigned_fallback<bool> {
	typedef bool type;
};

template <typename T>
struct _make_unsigned_fallback<T,
	typename std::enable_if<std::is_integral<T>::value>::type> {
	typedef typename std::make_unsigned<T>::type type;
};

template <typename T>
inline T const& _to_unsigned(T const& t,
    typename std::enable_if<!std::is_integral<T>::value>::type* = 0) {
	return t;
}

template <typename T>
inline auto _to_unsigned(T t)
	-> typename _make_unsigned_fallback<T>::type {
	return t;
}

template <typename Traits, typename T>
inline T const& _to_char(T const& t,
    typename std::enable_if<!std::is_same<
    typename Traits::int_type, T>::value>::type* = 0) {
	return t;
}

template <typename Traits, typename T>
inline auto _to_char(T t,
    typename std::enable_if<std::is_same<
    typename Traits::int_type, T>::value>::type* = 0)
	-> typename Traits::char_type {
	return Traits::to_char_type(t);
}

template <typename Traits, typename T>
inline T const& _to_int(T const& t,
    typename std::enable_if<!std::is_same<
    typename Traits::char_type, T>::value and
    not _accept_narrow<typename Traits::char_type, T>::value>::type* = 0) {
	return t;
}

template <typename Traits, typename T>
inline auto _to_int(T t,
    typename std::enable_if<std::is_same<
    typename Traits::char_type, T>::value>::type* = 0)
	-> typename Traits::int_type {
	return Traits::to_int_type(t);
}

template <typename Traits, typename T>
inline auto _to_int(T t)
	-> typename _accept_narrow<typename Traits::char_type, T>::int_type {
	return t;
}

template <typename T>
inline auto _streamsize_or_not(T const&,
    typename std::enable_if<!std::is_convertible<T,
    std::streamsize>::value>::type* = 0)
	-> std::pair<bool, std::streamsize> {
	return { false, 0 };
}

inline auto _streamsize_or_not(std::streamsize t)
	-> std::pair<bool, std::streamsize> {
	return { true, t };
}

template <typename Iter, typename Facet>
inline int _parse_int(Iter& b, Iter& e, Facet const& fac) {
	int n = 0;
	for (char c; b != e and (c = _to_narrow_digit(*b, fac)); ++b) {
		n *= 10;
		n += c - '0';
	}
	return n;
}

template <typename Iter, typename Facet>
inline int _parse_position(Iter& b, Iter& e, Facet const& fac) {
	auto ob = b;
	int n = _parse_int(b, e, fac);
	if (n == 0 or b == e or *b != fac.widen('$')) {
		b = ob;
		return 0;
	}
	++b;
	return n;
}

template <typename Stream>
inline auto _flags_for_output(Stream const& out) -> decltype(out.flags()) {
	using os = Stream;
	return out.flags() & os::unitbuf;
}

template <typename CharT>
struct _padding {
	template <typename Stream>
	explicit _padding(Stream const& s) :
		precision_(s.precision()), fill_(s.fill()) {}

	std::streamsize	precision_;
	CharT		fill_;
	bool		align_sign_ = false;
};

template <typename Stream>
struct _padding_guard {
	_padding_guard(Stream& s, _padding<decltype(s.fill())> pad) :
		stream_(s), pad_(s) {
		stream_.precision(pad.precision_);
		stream_.fill(pad.fill_);
	}
	~_padding_guard() {
		stream_.fill(pad_.fill_);
		stream_.precision(pad_.precision_);
	}

	_padding_guard(_padding_guard const&) = delete;
	_padding_guard& operator=(_padding_guard const&) = delete;

private:
	Stream&					stream_;
	_padding<decltype(stream_.fill())>	pad_;
};

template <typename Stream, typename T>
class _outputter;

template <typename Stream, typename T>
inline auto _output(Stream& out, T const& t)
	-> _outputter<Stream, T> {
	return { out, t };
}

template <typename Stream, typename T>
class _outputter {
	template <typename _Stream, typename _T>
	friend class _outputter;

	typedef typename std::decay<T>::type Tp_;
	typedef typename std::conditional<std::is_pointer<Tp_>::value,
			typename std::add_pointer<
			typename std::remove_const<
			typename std::remove_pointer<Tp_>::type>::type>::type,
		Tp_>::type RealT;

	Stream& out_;
	T const& t_;

public:
	typedef typename Stream::char_type	char_type;
	typedef typename Stream::traits_type	traits_type;
	typedef decltype(out_.flags())		fmtflags;
	typedef _padding<decltype(out_.fill())>	padding;

	_outputter(Stream& out, T const& t) : out_(out), t_(t) {}

	Stream& with(fmtflags fl, padding pad) {
		return _with(fl, pad, identity<RealT>());
	}

	template <typename _U = RealT>
	auto with_aligned_sign(fmtflags fl, padding pad)
		-> typename std::enable_if<
		!std::is_arithmetic<_U>::value, Stream&>::type {
		return _with(fl, pad, identity<RealT>());
	}

	template <typename _U = RealT>
	auto with_aligned_sign(fmtflags fl, padding pad)
		-> typename std::enable_if<
		std::is_arithmetic<_U>::value, Stream&>::type {
		using os = std::basic_ostringstream<char_type, traits_type>;

		os dummy_out;
		fl |= os::showpos;

		// simulate `out_'
		dummy_out.width(out_.width(0));
		dummy_out.fill(out_.fill());
		dummy_out.imbue(out_.getloc());
		_output(dummy_out, t_)._with(fl, pad, identity<RealT>());
		auto s = dummy_out.str();
		auto i = s.find(out_.widen('+'));
		if (i != decltype(s)::npos)
			s[i] = dummy_out.fill();
		out_.write(s.data(), s.size());
		return out_;
	}

private:
	template <typename _T>
	Stream& _with(fmtflags fl, padding pad, identity<_T>,
	    typename std::enable_if<!std::is_arithmetic<_T>::value or
	    _accept_narrow<traits_type, _T>::value>::type* = 0) {
		return _output__(fl, pad, t_);
	}

	template <typename _T>
	Stream& _with(fmtflags fl, padding pad, identity<_T>,
	    typename std::enable_if<std::is_floating_point<_T>::value
	    >::type* = 0) {
		if (fl & Stream::internal and !std::isfinite(t_))
			pad.fill_ = out_.fill();
		return _output__(fl, pad, t_);
	}

	template <typename _T>
	Stream& _with(fmtflags fl, padding pad, identity<_T>,
	    typename std::enable_if<std::is_integral<_T>::value and
	    not _accept_narrow<traits_type, _T>::value>::type* = 0) {
		return _output_int__(fl, pad);
	}

	Stream& _with(fmtflags fl, padding pad, identity<char_type>) {
		return _output__(fl, pad, t_);
	}

	Stream& _with(fmtflags fl, padding pad, identity<char_type *>) {
		return _output_chars__(fl, pad);
	}

	template <typename _CharT>
	Stream& _with(fmtflags fl, padding pad, identity<_CharT *>,
	    typename _accept_narrow<char_type, _CharT>::char_type* = 0) {
		return _output_chars__(fl, pad);
	}

	Stream& _output_int__(fmtflags fl, padding pad) {
		using os = std::basic_ostringstream<char_type, traits_type>;

		if (fl & os::showbase and fl & os::oct and pad.precision_ < 1)
			pad.precision_ = 1;
		if (pad.precision_ == 0 and t_ == 0)
			return _output__(fl, pad, "");
		if (pad.precision_ <= 1)
			return _output__(fl, pad, t_);

		int w = fl & os::hex ? _lexical_width<16>(t_) :
			fl & os::oct ? _lexical_width<8>(t_) +
			!!(fl & os::showbase) :
			_lexical_width<10>(t_);
		if (pad.precision_ <= w)
			return _output__(fl, pad, t_);

		os dummy_out;

		// print without width
		dummy_out.imbue(out_.getloc());
		_output(dummy_out, t_)._output__(fl, pad, t_);
		auto s = dummy_out.str();
		s.insert(s.size() - w, pad.precision_ - w, out_.widen('0'));
		return _output__(fl, pad, s);
	}

	Stream& _output_chars__(fmtflags fl, padding pad) {
		typedef typename std::remove_pointer<RealT>::type _CharT;

		if (pad.precision_ < 0)
			return _output__(fl, pad, t_);

		size_t n = 0;
		auto i = t_;
		for (; *i and n < static_cast<size_t>(pad.precision_); ++i)
			++n;
		if (*i != 0)
			return _output__(fl, pad,
			    std::basic_string<_CharT>(t_, n).data());
		else
			return _output__(fl, pad, t_);
	}

	template <typename _T>
	Stream& _output__(fmtflags fl, padding pad, _T const& t) {
		_padding_guard<Stream> _(out_, pad);
		out_.flags(fl);
		out_ << t;
		return out_;
	}
};

template <typename CharT, typename Traits>
struct _put_fmtter;

template <typename CharT, typename Traits, typename... Args>
inline auto _put_fmt(std::basic_ostream<CharT, Traits>& out, Args... args)
	-> _put_fmtter<CharT, Traits> {
	return _put_fmtter<CharT, Traits>(out, args...);
}

enum class spec {
	none,
	to_unsigned,
	to_char,
	to_int,
};

template <typename CharT, typename Traits>
struct _put_fmtter {
	using os = std::basic_ostream<CharT, Traits>;

	typedef typename os::fmtflags	fmtflags;
	typedef _padding<CharT>		padding;

	explicit _put_fmtter(os& s) : out(s), argN(0) {}

	template <typename... T>
	auto from(_fmt_put<CharT, T...>& t) -> os&
	{
		using std::begin; using std::end;

		auto& b = begin(t);
		auto i = b;
		auto& fac = std::use_facet<std::ctype<CharT>>(out.getloc());

		if (b == end(t))
			return out;
		i = std::find(b, end(t), out.widen('%'));
		out.write(&*b, i - begin(t));
		if (i == end(t)) {
			// ignore excess arguments
			return out;
		}
		b = ++i;
		if (b == end(t)) {
			out.setstate(os::failbit);	// only 1 trailing %
			return out;
		}

		if (*b == out.widen('%')) {
			++b;
			out.put(out.widen('%'));
			return from(t);
		}

		auto sp = spec::none;
		auto fl = _flags_for_output(out);
		auto pad = padding(out);
		pad.precision_ = -1;

		auto _ignore_zero_padding = [&] {
			fl &= ~os::internal;
			pad.fill_ = out.fill();
		};

		auto _bad_indexing = [&](int i) {
			if (sequential)
				return i != 0 or argN >= int(sizeof...(T));
			else
				return i == 0 or i > int(sizeof...(T));
		};

		{
			auto ob = b;
			int ti = _parse_int(b, end(t), fac);
			if (ti == 0 or b == end(t) or
			    (*b != fac.widen('$') and
			     *b != fac.widen('%'))) {
				ti = 0;
				b = ob;
			}
			if (argN == 0)	// access is not known yet
				sequential = (ti == 0);
			if (_bad_indexing(ti)) {
				out.setstate(os::failbit);
				return out;
			}
			if (ti == 0)
				++argN;
			else
			{
				argN = ti;

				if (*b++ == out.widen('%')) {
					visit1_at(argN, [&](auto&& x)
					    { _output(out, x).with(fl, pad); },
					    t);
					return from(t);
				}
			}
		}

		parse_flags:
		if (b == end(t)) {
			out.setstate(os::failbit);	// incomplete spec
			return out;
		}

		switch (out.narrow(*b, 0)) {
		case '-':
			_ignore_zero_padding();
			fl |= os::left;
			break;
		case '+':
			pad.align_sign_ = false;
			fl |= os::showpos;
			break;
		case ' ':
			if (!(fl & os::showpos))
				pad.align_sign_ = true;
			break;
		case '#':
			fl |= os::showbase | os::showpoint;
			break;
		case '0':
			if (!(fl & os::left)) {
				fl |= os::internal;
				pad.fill_ = out.widen('0');
			}
			break;
		default:
			goto parse_width;
		}
		++b;
		goto parse_flags;

		parse_width:
		if (_to_narrow_digit(*b, fac))
			out.width(_parse_int(b, end(t), fac));
		else if (*b == out.widen('*')) {
			++b;
			int ti = _parse_position(b, end(t), fac);
			if (_bad_indexing(ti)) {
				out.setstate(os::failbit);
				return out;
			}
			if (ti == 0)
				ti = argN++;

			std::pair<bool, std::streamsize> sz;
			visit1_at(ti, [&](auto&& x)
			    { sz = _streamsize_or_not(x); }, t);
			if (!sz.first) {
				out.setstate(os::failbit);
				return out;
			}
			if (sz.second >= 0)
				out.width(sz.second);
			else {
				// '-' clears '0'
				_ignore_zero_padding();
				fl |= os::left;
				out.width(-sz.second);
			}
		}

		if (b == end(t)) {
			out.setstate(os::failbit);	// incomplete spec
			return out;
		}

		// precision defaults to zero with a single '.'
		if (*b == out.widen('.')) {
			if (++b == end(t)) {
				out.setstate(os::failbit);
				return out;
			}
			if (*b == out.widen('*')) {
				++b;
				int ti = _parse_position(b, end(t), fac);
				if (_bad_indexing(ti)) {
					out.setstate(os::failbit);
					return out;
				}
				if (ti == 0)
					ti = argN++;

				std::pair<bool, std::streamsize> sz;
				visit1_at(ti, [&](auto&& x)
				    { sz = _streamsize_or_not(x); }, t);
				if (!sz.first) {
					out.setstate(os::failbit);
					return out;
				}
				if (sz.second >= 0)
					pad.precision_ = sz.second;
			} else
				pad.precision_ = _parse_int(b, end(t), fac);
		}

		if (b == end(t)) {
			out.setstate(os::failbit);	// incomplete spec
			return out;
		}

		// ignore all length modifiers
		switch (auto c = out.narrow(*b, 0)) {
		case 'h': case 'l':
			if (++b == end(t)) {
				out.setstate(os::failbit);
				return out;
			}
			c = out.narrow(*b, 0);
			if (c == 'h' or c == 'l')
				++b;
			break;
		case 'j': case 'z': case 't': case 'L':
			++b;
			break;
		}

		if (b == end(t)) {
			out.setstate(os::failbit);	// incomplete spec
			return out;
		}

		// type-safe conversions are considered
		switch (out.narrow(*b, 0)) {
		case 'p':
			break;
		case 'X':
			fl |= os::uppercase;
		case 'x':
			fl |= os::hex;
			sp = spec::to_unsigned;
			if (pad.precision_ >= 0)
				_ignore_zero_padding();
			break;
		case 'o':
			fl |= os::oct;
			sp = spec::to_unsigned;
			if (pad.precision_ >= 0)
				_ignore_zero_padding();
			break;
		case 'E':
			fl |= os::uppercase;
		case 'e':
			fl |= os::scientific;
			break;
		case 'F':
			fl |= os::uppercase;
		case 'f':
			fl |= os::fixed;
			break;
		case 'G':
			fl |= os::uppercase;
		case 'g':	/* floatfield == 0 */
			break;
		case 'A':
			fl |= os::uppercase;
		case 'a':
			fl |= os::fixed | os::scientific;
			break;
		case 'u':
			fl |= os::dec;
			sp = spec::to_unsigned;
			if (pad.precision_ >= 0)
				_ignore_zero_padding();
			break;
		case 'd':
			fl |= os::dec;
		case 'i':	/* basefield == 0 */
			sp = spec::to_int;
			if (pad.precision_ >= 0)
				_ignore_zero_padding();
			break;
		case 's':
			fl |= os::boolalpha;
			break;
		case 'c':
			sp = spec::to_char;
			break;
		default:
			out.setstate(os::failbit);	// bad format string
			return out;
		}

		switch (tolower(out.narrow(*b, 0))) {
		case 'p': case 's': case 'c':
			visit1_at(argN, [&](auto&& x) {
				typedef std::decay_t<decltype(x)> type_i;
				if (std::is_integral<type_i>::value)
					pad.precision_ = -1;
			    }, t);
			pad.align_sign_ = false;
			break;
		case 'e': case 'f': case 'g':
			if (pad.precision_ < 0)
				pad.precision_ = 6;
		case 'a':
			visit1_at(argN, [&](auto&& x) {
				typedef std::decay_t<decltype(x)> type_i;
				if (std::is_integral<type_i>::value)
					pad.precision_ = -1;
				if (!std::is_floating_point<type_i>::value)
					pad.align_sign_ = false;
			    }, t);
			break;
		case 'd': case 'i': case 'u': case 'o': case 'x':
			visit1_at(argN, [&](auto&& x) {
				typedef std::decay_t<decltype(x)> type_i;
				if (!std::is_integral<type_i>::value)
					pad.align_sign_ = false;
			    }, t);
		}
		++b;

		switch (sp) {
		case spec::none: {
			visit1_at(argN, [&](auto x) {
				auto v = _output(out, x);
				pad.align_sign_ ?
				    v.with_aligned_sign(fl, pad) :
				    v.with(fl, pad);
			    }, t);
			break;
		}
		case spec::to_int: {
			visit1_at(argN, [&](auto x) {
				auto v = _output(out, _to_int<Traits>(x));
				pad.align_sign_ ?
				    v.with_aligned_sign(fl, pad) :
				    v.with(fl, pad);
			    }, t);
			break;
		}
		case spec::to_unsigned:
			visit1_at(argN, [&](auto x) {
				_output(out,
				    fl & os::dec ?
				    _to_unsigned(_to_int<Traits>(x)) :
				    _to_unsigned(x)).with(fl, pad);
			    }, t);
			break;
		case spec::to_char:
			visit1_at(argN, [&](auto x) {
				_output(out,
				    _to_char<Traits>(x)).with(fl, pad);
			    }, t);
			break;
		}

		return from(t);
	}

private:
	os&	out;
	int	argN;
	bool	sequential;
};

template <typename CharT, typename Traits, typename... T>
inline auto operator<<(std::basic_ostream<CharT, Traits>& out,
    _fmt_put<CharT, T...> t) -> decltype(out)
{
	_unformatted_guard<std::basic_ostream<CharT, Traits>> ok(out);

	if (ok)
		_put_fmt(out).from(t);
	return out;
}

}

#endif
