#include <boost/format.hpp>
#include <ioformat>
#include <iomanip>
#include <cstdio>

using std::cout;
using std::left;
using std::setprecision;
using boost::format;
using std::experimental::putf;

int main() {
	cout << putf("hello, %s\n", "world");
	cout << format("The answer:%5d\n") % 42;
	cout << putf("The answer:%5d\n", 42);
	cout << putf("The answer:%*d\n", 5, 42);
	cout << putf("The answer:%2$*1$d\n", 5, 42);
	cout << putf("The answer:% -.4d\n", 42);
	printf("The answer:% -.4d\n", 42);
	cout << format("The answer:% -.4d\n") % 42;
	cout << putf("The answer:% -.4f\n", 42);
	cout << "The answer:" << left << setprecision(4) << 42 << "\n";
}
