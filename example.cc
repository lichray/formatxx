#include "putf.h"
#include <iostream>
#include <cmath>
#include <cstdio>

using std::cout; using std::wcout;
using stdex::putf;

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
	wcout << putf(L"%-+012d|%+014p\n", 1234567, &f);
	wprintf(L"%-+012d|%+014p\n", 1234567, &f);
	cout << putf("%12.4s|%14.2s|%.s\n", s.data(), s, s.data());
	printf("%12.4s|%14.2s|%.s\n", s.data(), s.data(), s.data());
	return 0;
}
