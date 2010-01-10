all:

test:
	cd examples; make test-ghc

commit:
	darcs record -a -m "Committed from the Makefile"
	darcs push -a
