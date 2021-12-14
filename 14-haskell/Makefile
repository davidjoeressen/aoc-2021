.PHONY: test
test: main sample input
	./main sample
	./main input

main: main.hs
	stack ghc main.hs
