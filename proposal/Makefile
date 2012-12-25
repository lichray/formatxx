# ccconf examples CXX=g++48 CXXFLAGS+=-std=c++11 -Wall -g -I.. -I/usr/local/include
CXXFLAGS = -std=c++11 -Wall -g -I.. -I/usr/local/include  
CXX      = g++48  

.PHONY : all clean
all : examples
clean :
	rm -f examples examples.o tags

tags : *.h examples.cc 
	ctags *.h examples.cc 

examples : examples.o
	${CXX} ${LDFLAGS} -o examples examples.o
examples.o: examples.cc
