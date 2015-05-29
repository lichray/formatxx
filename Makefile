# ccconf example CXX=g++49 CXXFLAGS+=-std=c++11 -Wall
CXXFLAGS = -std=c++1y -Wall -g -I. -I/usr/local/include
CXX      = g++49

.PHONY : all clean
all : example
clean :
	rm -f example example.o

example : example.o
	${CXX} ${LDFLAGS} -o example example.o
example.o: example.cc putf.h __format_base.h
