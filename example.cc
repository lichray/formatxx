#include <iostream>
#include <ioformat>
#include <cmath>
#include <cstdio>
#include <array>

using std::cout; using std::wcout;
using std::experimental::putf;
using std::experimental::vputf;

int main() {
	auto f = NAN;
	std::string s("astring");
	cout << putf("%x %p %#o %#X\n", -1, &f, -1, -1);
	printf("%x %p %#o %#X\n", -1, &f, -1, -1);
	cout << putf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	printf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	f = 3.1415;
	cout << vputf("%e %f %g %a %E %F %G %A\n", std::tie(f, f, f, f, f, f, f, f));
	printf("%e %f %g %a %E %F %G %A\n", f, f, f, f, f, f, f, f);
	cout << putf("%i %d %u %i %d %u\n", -1, -1, -1, -1, -1, -1);
	printf("%i %d %u %i %d %u\n", -1, -1, -1, -1, -1, -1);
	cout << vputf("%c %d %#.2f\n", std::make_tuple(64, 64, 0.1));
	printf("%c %d %#.2f\n", 64, 64, 0.1);
	wcout << putf(L"% 012d|%.f\n", 1234567, f);
	wprintf(L"% 012d|%.f\n", 1234567, f);
	wcout << vputf(L"%-+012d|%+014p\n", std::make_pair(1234567, &f));
	wprintf(L"%-+012d|%+014p\n", 1234567, &f);
	cout << putf("%12.4s|%14.2s|%.s\n", s.data(), s, s.data());
	printf("%12.4s|%14.2s|%.s\n", s.data(), s.data(), s.data());
	std::array<int, 7> args {{ -12, 6, 64, 12, 64, -6, 64 }};
	cout << vputf("%*.*d|%#.*X|% 0*d\n", args);
	printf("%*.*d|%#.*X|% 0*d\n", -12, 6, 64, 12, 64, -6, 64);
	cout << putf("%1$d:%2$.*3$d:%4$*4$.*3$d\n", 12, 6, 2, 6);
	printf("%1$d:%2$.*3$d:%4$*4$.*3$d\n", 12, 6, 2, 6);
	//cout << vputf("%#x %#x %#x %#x\n", "bye!");
	return 0;
}
