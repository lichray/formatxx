<!-- maruku -o printf.html printf.md -->

	Document number:	Nnnnn=yy-nnnn
	Date: 			2012-12-19
	Project:		Programming Language C++, Library Working Group
	Reply-to:		Zhihao Yuan <lichray at gmail dot com>

# A printf-like Interface for the Streams Library

* TOC
{:toc}

## Overview

      cout << putf("hello, %s\n", "world");

Printf defines the most widely used syntax to format a text output.  It exists
in C, Perl, Python and even Java&trade;, and is available from Qt to
Boost.Format`[1]`, but not C++ standard library.  This proposal tries to
define such an interface based on the `printf` function defined by C`[2]` for
the C++ I/O streams library, with the error handling policy and the type safety
considered.

## Impact on the Standard

The proposed new header `<ioformat>` makes no changes to the existing
interface of the streams library, other than an `operator<<(basic_ostream)`
overload to print the unspecified return value of a new `std::putf` function.
However, the proposed formatting features are not parallel to those provided by
the existing streams library.  For short, the I/O manipulators can be fully
replaced by the member functions of `ios_base`, while `std::putf` can not.

The additional formatting features supported by `std::putf` are:

 - Empty sign `"% d"`.
 - Hexfloat with precision `"%.4a"`.
 - Integer with precision (minimal digits) `"%#5.2x"`.
 - String with precision (truncation, only for C-style strings) `"%.4s"`.

## Design Decisions

The idea is to define a portable and readable syntax to enable the
extensible formatting of the streams library, while allowing an implementation
to perform any formatting without any extra buffering comparing to the `<<`
operator.

### Syntax

The syntax from printf in C is preserved as much as possible.  Such an
syntax is:

 - Compatible with C; works as a drop-in replacement of `printf` (except `%n`).
 - Compatible with the legacy syntax supported by Boost.Format.

For example, both of the following

      cout << format("The answer:%5d\n") % 42;  // boost.format
      cout << putf("The answer:%5d\n", 42);     // std::experimental::putf

print

      The answer:   42

The _width_ `5` can be parameterized:

      cout << putf("The answer:%*d\n", 5, 42);  // same effect

This mechanism is supported by both C and POSIX, but not Boost.Format.

POSIX`[4]` style positional arguments are added because they are necessary
for i18n.

So the example above can be rewrote into:

      cout << putf("The answer:%2$*1$d\n", 5, 42);  // same effect

The `%n` specification is dropped because of the security problem (and its
weird semantics); no known printf fork (in Java&trade;, Python, Boost.Format,
etc.) supports it.

C++ streams style error handling policy and type safety requirements are
satisfied with the highest priority.  However, that makes the _length
modifiers_ (`hh`, `h`, `l`, `ll`, `j`, `z`, `t`, `L`) unneeded.  The proposed
solution is to ignore them, like Boost.Format and Python`[3]`, while the only
difference is that, we completely ignore all of them according to the C
standard, not just a subset.

### Extensibility

A subset of the printf format specification can be translated into a
combination of the formatting properties (`flags()`, `width()`, `precision()`
and `fill()`) of an output stream.  To balance the standard compliance and the
extensibility, this proposal distinguishes the arguments to be printed into:

 - _internally formattable_, which have the same formatting as if they are
   formatted by `snprintf` or a wide character equivalence given the same
   format specifications with a fitted length modifier, and
 - _potentially formattable_, which will be outputted by the `<<` operator
   with the translated formatting properties set up on the output stream.

If an argument is internally formattable by a format specification, then C's
formatting is fully supported.  For example, the following

      cout << putf("The answer:% -.4d\n", 42);  // empty sign, left alignment, 4 minimal digits

has the same printing result as

      printf("The answer:% -.4d\n", 42);

which gives

      The answer: 0042

, while Boost.Format gives

      The answer: 42

without an integer precision support.

But if an argument is potentially formattable by a specification, the
following

      cout << putf("The answer:% -.4f\n", 42);  // expects a floating point

has the same printing result as

      cout << "The answer:" << left << setprecision(4) << 42 << "\n"

which gives

      The answer:42

since there is no "empty sign" support in the streams library.

A detailed description is available in [Formatting](#formatting).

## Technical Specifications

*The description below is based on POSIX`[4]`.*

`std::putf` takes a format string, followed by zero or more arguments.  A
format string is composed of zero or more directives:  _ordinary characters_,
which are copied unchanged to the output stream, and _format specifications_,
each of which expects zero or more arguments.

An empty format specification `%%` matches no argument; a `'%'` character is
printed without formatting.

A numbered format specification introduced by `"%`_`n`_`$"` matches the _n_th
argument in the argument list, where _n_ is a decimal integer.

An unnumbered format specification introduced by `'%'` matches the first
unmatched argument in the argument list.

Matching an out-of-range argument in a format string results in an error
described in [Error handling](#error_handling), while the unmatched arguments
are ignored.  An argument can be matched multiple times by a format string
of the numbered format specifications.

The character sequence `"%`_`n`_`$"` or the `'%'` character, introducing a
format specification, has the following appear in sequence:

 - Zero or more _flags_ (in any order).
 - An optional minimum _field width_, which takes either a parameterized length
   ( `'*'` or `"*`_`n`_`$"`), described below, or a decimal integer.
 - An optional _precision_, which takes the form of a period ( `'.'` )
   followed either by a parameterized length ( `'*'` or `"*`_`n`_`$"` ),
   described below, or an
   optional decimal digit string, where a null digit string is treated as zero.
 - An optional length modifier (ignored).
 - A _type hint_ character that indicates the type of the matched argument.

A field width, or precision, or both, may be indicated by a numbered
parameterized length ( `"*`_`n`_`$"` ), which is allowed within a numbered
format specification, or an unnumbered parameterized length ( `'*'` ), which is
allowed within an unnumbered format specification.  In such cases an argument
of type `streamsize` supplies the field width or precision.  A numbered
parameterized length matches the _n_th argument in the argument list,  where
_n_ is a decimal integer.  The unnumbered parameterized lengths, in their order
of appearance, match the unmatched arguments in the argument list, before the
format specification they belong to. 
A negative field width is taken as a `'-'` flag followed
by a positive field width.  A negative precision is taken as if the precision
were omitted.

A format string can contain either numbered format specifications, or
unnumbered format specifications, but not both.  Mixing numbered and
unnumbered specifications or parameterized lengths result in an error described
in [Error handling](#error_handling).  The empty format specification `%%` can
be mixed with any specifications.

### Header `<ioformat>`

      namespace std {
      namespace experimental {

        // types _Ts1_ and _Ts2_ are sets of implementation types which are distinguishable for different T...

        template <typename CharT, typename... T>
        _Ts1_ putf(CharT const *fmt, T const&... t);

        template <typename CharT, typename Traits, typename Allocator, typename... T>
        _Ts2_ putf(basic_string<CharT, Traits, Allocator> const& fmt, T const&... t);

        template <typename CharT, typename... T>
        auto sputf(CharT const *fmt, T const&... t)
            -> basic_string<CharT>;

        template <typename CharT, typename Traits, typename Allocator, typename... T>
        auto sputf(basic_string<CharT, Traits, Allocator> const& fmt, T const&... t)
            -> basic_string<CharT, Traits>;

        template <typename CharT, typename Traits, typename... T>
        auto operator<<(basic_ostream<CharT, Traits>& os, _Ts1_or_Ts2_ bundle)
            -> decltype(os);

      }}

The output functions of the return values of `std::putf` do formatted output,
but behavior like the _unformatted output functions_.  Specifically, `flags()`,
`width()`, `precision()` and `fill()` of the output stream are preserved when
the flow of control leaves these functions, but may be changed during the
execution.  Changing the return values of these members before the execution
takes no effect to the printing, except:

 - `flags() & ios_base::unitbuf` may change the buffering behavior.
 - `fill()` works as the default padding character.

The `std::sputf` overloads take the same arguments as `std::putf`, but return
the formatted results as the objects of the `basic_string` instantiations.

### Error handling

An output function of a return value of `std::putf` may encounter the
following kinds of errors found in the return value:

 - A format specification is syntactically invalid.
 - A format specification expects an argument that does not appear
   in the argument list.
 - Mixing numbered and unnumbered format specifications or parameterized
   lengths.
 - The argument matched a parameterized length is not convertible to
   `streamsize`.

The output function set `ios_base::failbit` on the output stream when
one of the errors is encountered, and then can return.  The well matched format
specifications, as well as the ordinary characters, if any, before the format
specification that fails, must be formatted and wrote to the output stream
before the function returns.

### Formatting

For a `basic_ostream<CharT, Traits>` and a given format description, the
matched argument is _internally formattable_ if:

 - the type hint is `d`, `i`, `o`, `u`, `x`, or `X`, and the argument is an
   integer, or
 - the type hint is `a`, `A`, `e`, `E`, `f`, `F`, `g`, or `G`, and the
   argument is a floating-point number, or
 - the type hint is `p`, and the argument is a pointer, or
 - the type hint is `c`, and the argument is `char`, `CharT`, or
   `Traits::int_type`, or `signed char`/`unsigned char` if `CharT` is `char`,
   or
 - the type hint is `s`, and the argument is `const char*`, `const CharT*`,
   or `const signed char*`/`const unsigned char*` if `CharT` is `char`.

*\[Note: An internally formattable argument has an `operator<<` overload,
member or non-member, in the `<ostream>` header, and can be printted by
`printf` without a type-unsafe conversion.  This note also applys to
`Traits::int_type`, considering its underlying type.  --end note\]*

Otherwise, the argument is _potentially formattable_.

If an _internally formattable_ argument is an unsigned integer and the type
hint is `d` or `i`, the argument is printed as if it is formatted by `snprintf`
or a wide character equivalence, which conceptually uses a default padding
character of `os.fill()`, given the same _flags_, _field-width_, and
_precision_, if any, respectively, followed by a fitted length modifier, if
needed, and a _type hint_ of `u`.  Otherwise, the argument is printed as if it
is formatted by `snprintf` or a wide character equivalence, which conceptually
uses a default padding character of `os.fill()`, given the same
_flags_, _field-width_, and _precision_, if any, respectively, followed by a
fitted length modifier, if needed, and the same _type hint_.  *\[Note: `u`,
`o`, `x`, `X` convert a signed argument to unsigned, while `d` and `i` do not
convert an unsigned argument to signed. --end note\]*

If the argument is _potentially formattable_, `width()` and `precision()` of
the output stream are defaulted to `0` and `-1`, respectively.  The `flags()`
member is defaulted to `os.flags() & ios_base::unitbuf`, and the `fill()`
member is defaulted to the saved fill character of the output stream before
entering the current output function.

For a given format description, if the argument is _potentially formattable_,
the _flag_ characters and their effects on the output stream are:

 - **`-`** sets `ios_base::left`.
 - **`+`** sets `ios_base::showpos`.
 - _space_ has no effect.
 - **`#`** sets `ios_base::showbase` and `ios_base::showpoint`.
 - **`0`** sets `fill()` to `'0'` and sets `ios_base::internal`, only if the
   `'-'` flag does not appear in the flags, and a precision is not specified if
   the type hint is `d`, `i`, `o`, `u`, `x`, or `X`.

Under the same preconditions, the _field-width_ field, if any, sets the
`width()` member of the output stream; and the _precision_ field, if any, sets
the `precision()` member of the output stream.  *\[Note: The cases of a
negative _field-width_ or _precision_ are described in [Technical
Specifications](#technical_specifications). --end note\]*

Under the same preconditions, the _type hint_ characters and their effects on
the output stream are:

 - **`d`** sets `ios_base::dec`.
 - **`i`** has no effect (`os.flags() & ios_base::basefield == 0`).
 - **`u`** sets `ios_base::dec`.
 - **`o`** sets `ios_base::oct`.
 - **`x`** sets `ios_base::hex`.
 - **`X`** sets `ios_base::hex | ios_base::uppercase`.
 - **`f`** sets `ios_base::fixed`.
 - **`F`** sets `ios_base::fixed | ios_base::uppercase`.
 - **`e`** sets `ios_base::scientific`.
 - **`E`** sets `ios_base::scientific | ios_base::uppercase`.
 - **`g`** has no effect (`os.flags() & ios_base::floatfield == 0`)
 - **`G`** sets `ios_base::uppercase`.
 - **`a`** sets `ios_base::fixed | ios_base::scientific`.
 - **`A`** sets `ios_base::fixed | ios_base::scientific | ios_base::uppercase`.
 - **`c`** has no effect.
 - **`s`** sets `ios_base::boolalpha`.
 - **`p`** has no effect.

And then, the _potentially formattable_ argument, namely `t`, is printed by
calling `os << t`.

### Wording

This is an initial report; a wording can be prepared after a further
discussion.

## Sample Implementation

A sample implementation is available at
<https://github.com/lichray/formatxx/tree/proposal>

One known defect in this implementation is that the `%a` and `%A` format
specifications ignore the precision when printing a floating point argument.

### Performance notes

The additional runtime performance costs comparing with the streams library
are caused by parsing the format string and creating the formatting guards (to
restore the flags, precision, etc., after formatting each specifications,
exception-safely).  In addition, to access a positional argument numbered
_N_, _N - 1_ empty recursions are required to locate the correct template
instantiation.

In the sample implementation, some extra copying are involved to emulate
`printf`'s formatting features using streams.  However, the _internally
formattable_ arguments are internally supported by the streams library, so a
standard library implementation must be able to avoid these costs.  For
example, to print a string with precision, the sample implementation has to
copy the string, while `libstdc++` already has an internal interface
`__ostream_insert()` which takes a size parameter.  These costs are not
shown by the benchmark below, and Boost.Format does the same thing, actually.

Here is a benchmark using Boost.Format's test code, release mode:

Non-positional arguments/normal:

      printf time         :0.367188
      ostream time        :0.59375,  = 1.61702 * printf 
      format time         :2.125,  = 5.78723 * printf ,  = 3.57895 * nullStream 
      std::putf time      :0.90625,  = 2.46809 * printf ,  = 1.52632 * nullStream 

Positional arguments/normal:

      printf time         :0.414062
      ostream time        :0.59375,  = 1.43396 * printf 
      format time         :2.11719,  = 5.11321 * printf ,  = 3.56579 * nullStream 
      std::putf time      :1.00781,  = 2.43396 * printf ,  = 1.69737 * nullStream 

Environment:

      FreeBSD 8.3-STABLE amd64
      g++ 4.8.0 20121209
      Boost 1.48.0

*Explanations*:

*The two test cases take the same amount of arguments, and have the same
formatting results.  The streams library has no such "positional arguments",
so I reordered the arguments by hand. *

*"normal" means the locale is turned on.  However, I did not see a stable
difference between `normal` and `no_locale`.*

*The format object of boost can be
reused, which brings a performance increase around %17.  Such a "feature"
is not applicable to `printf` or `std::putf`, so I did not include them.*

## Future Issues

Is an `scanf` equivalence, e.g., `std::getf`, worth to be added?  If so, what
about an `sscanf` equivalence, and what its prototype should be?

## Acknowledgments

Andrew Sandoval, who gave me some suggestions on standard-compliance and
error handling.

Herb Sutter, who encouraged me to prepare the proposal, suggested me to add
the positional arguments, and even provided many suggestions and corrections
on the proposal.

Many people in the "std-proposals" mailing list: Jeffrey Yasskin, who
"enforced" me to add the positional arguments; Martin Desharnais, who gave me
the link about how to implement one; and many others.


## References

`[1]` The Boost Format library.
      <http://www.boost.org/doc/libs/1_52_0/libs/format/doc/format.html>

`[2]` The `fprintf` function.  _ISO/IEC 9899:2011_. 7.21.6.1.

`[3]` String Formatting Operations.  _The Python Standard Library_. 5.6.2.
      <http://docs.python.org/2/library/stdtypes.html#string-formatting>

`[4]` dprintf, fprintf, printf, snprintf, sprintf - print formatted output.
      _IEEE Std 1003.1-2008_.
      <http://pubs.opengroup.org/onlinepubs/9699919799/functions/printf.html>
