all:

test:
	cd examples; $(MAKE) test

commit:
	darcs record -a -m "See ChangeLog"
	darcs push -a
