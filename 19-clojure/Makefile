.PHONY: test
test: target/aoc-1.0-standalone.jar sample input
	java -jar target/aoc-1.0-standalone.jar sample input

target/aoc-1.0-standalone.jar: src/main.clj project.clj
	lein uberjar
