CFLAGS=-O3 -ffast-math -funroll-loops -static # -msse -march=athlon

all:	test_nn knn

cover_tree.o: cover_tree.cc cover_tree.h
	g++ -g -c -Wall $(CFLAGS) cover_tree.cc

point.o: point.cc point.h
	g++ -g -c -Wall $(CFLAGS) point.cc

test_nn:	point.o cover_tree.o stack.h point.h test_nn.cc
	g++ -Wall $(CFLAGS) -o test_nn test_nn.cc cover_tree.o point.o

knn:	point.o cover_tree.o stack.h point.h knn.cc
	g++ -g -Wall $(CFLAGS) -o knn knn.cc cover_tree.o point.o

clean:
	rm *.o
