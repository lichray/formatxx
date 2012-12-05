# ccconf example CXX=g++48 CXXFLAGS+=-std=c++11 -Wall
CXXFLAGS = -std=c++11 -Wall -g
CXX      = g++48  

.PHONY : all clean
all : example
clean :
	rm -f example example.o tags

tags : *.h example.cc 
	ctags *.h example.cc 

example : example.o
	${CXX} ${LDFLAGS} -o example example.o
example.o: example.cc putf.h __format_base.h
