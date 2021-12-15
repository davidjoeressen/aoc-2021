.PHONY: test
test: main sample input
	./main sample
	./main input

main: main.f90
	gfortran -o main main.f90
