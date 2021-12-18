.PHONY: test
test: main.jar input
	java -jar main.jar input

main.jar: main.kt
	kotlinc main.kt -include-runtime -d main.jar
