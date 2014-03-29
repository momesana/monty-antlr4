grammar:
	 antlr4 Monty.g4 && javac *.java

run-tests:
	# Due to a bug in grun, the following does not parse all of the passed files:
	# grun Monty compilationUnit tests/*.monty

	bash -c 'for file in tests/*.monty; do echo $$file; grun Monty compilationUnit $$file; done;'
