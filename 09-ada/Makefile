.PHONY: test
test: main sample input
	./main sample
	./main input

main: main.adb
	gnatmake -gnat2020 main.adb
	#gnatmake main.adb
