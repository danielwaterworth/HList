all:

test:
	cd examples; make test-ghc

commit:
	darcs record -a -m "See ChangeLog"
	darcs push -a
