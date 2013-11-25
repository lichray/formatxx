#include <boost/format.hpp>
#include <ioformat>
#include <iomanip>
#include <cstdio>

using std::cout;
using std::left;
using std::setprecision;
using boost::format;

namespace std {
using std::experimental::putf;
}

int main() {
	putf(cout, "hello, %s\n", "world");
	cout << format("The answer:%5d\n") % 42;
	putf(cout, "The answer:%5d\n", 42);
	putf(cout, "The answer:%*d\n", 5, 42);
	putf(cout, "The answer:%2$*1$d\n", 5, 42);
	putf(cout, "%2$s: %1$s\n", 42, "The answer");
	putf(cout, "%2%: %1%\n", 42, "The answer");
	putf(cout, "The answer:% -.4d\n", 42);
	printf("The answer:% -.4d\n", 42);
	cout << format("The answer:% -.4d\n") % 42;
	putf(cout, "The answer:% -.4f\n", 42);
	cout << "The answer:" << left << setprecision(4) << 42 << "\n";
}
