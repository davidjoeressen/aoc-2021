.PHONY: test
test: main sample input
	./main sample
	./main input

main: main.adb
	gnatmake main.adb
