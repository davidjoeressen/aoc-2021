.PHONY: test
test: main sample input
	./main sample
	./main input

main: main.ml
	ocamlopt -o main main.ml
