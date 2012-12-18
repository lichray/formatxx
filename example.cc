#include "putf.h"
#include <iostream>
#include <cmath>
#include <cstdio>

using std::cout; using std::wcout;
using std::experimental::putf;
using std::experimental::sputf;

int main() {
	auto f = NAN;
	std::string s("astring");
	cout << putf("%x %p %#o %#X\n", -1, &f, -1, -1);
	printf("%x %p %#o %#X\n", -1, &f, -1, -1);
	cout << putf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	printf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	f = 3.1415;
	cout << putf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	printf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	cout << putf("%i %d %u %i %d %u\n", -1, -1, -1, -1, -1, -1);
	printf("%i %d %u %i %d %u\n", -1, -1, -1, -1, -1, -1);
	cout << putf("%c %d %#.2f\n", 64, 64, 0.1);
	printf("%c %d %#.2f\n", 64, 64, 0.1);
	wcout << putf(L"% 012d|%.f\n", 1234567, f);
	wprintf(L"% 012d|%.f\n", 1234567, f);
	wcout << sputf(L"%-+012d|%+014p\n", 1234567, &f);
	wprintf(L"%-+012d|%+014p\n", 1234567, &f);
	cout << sputf("%12.4s|%14.2s|%.s\n", s.data(), s, s.data());
	printf("%12.4s|%14.2s|%.s\n", s.data(), s.data(), s.data());
	cout << putf("%*.*d|%#.*X|% 0*d\n", -12, 6, 64, 12, 64, -6, 64);
	printf("%*.*d|%#.*X|% 0*d\n", -12, 6, 64, 12, 64, -6, 64);
	return 0;
}
