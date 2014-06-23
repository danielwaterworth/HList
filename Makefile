all:

test:
	cabal test

commit:
	darcs record -a -m "See ChangeLog"
	darcs push -a
