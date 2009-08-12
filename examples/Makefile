##############################################################################
#
# Useful make targets
#
# make test        -- run all GHC- and Hugs-based test cases.
# make clean       -- remove all generated and temporary and backup files
# make ghci        -- start an HList ghci session
#


##############################################################################
#
# Some variables
#

ghci = ghci -i.. -fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances
ghc-favourite  = MainGhcGeneric1.hs
hugs-favourite = MainHugsTTypeable.hs


##############################################################################
#
# By default tell user to have a look at the Makefile's header.
#

all:
	@echo
	@echo "*****************************************************"
	@echo "* See the Makefile's header for reasonable targets. *"
	@echo "* Perhaps, you may want to run make test?           *"
	@echo "*****************************************************"
	@echo


##############################################################################
#
# Start a GHCI session for the favoured GHC model
#

ghci:
	ghci ${ghc-favourite}


##############################################################################
#
# Start a Hugs session for the favoured Hugs model
#

hugs:
	hugs -98 +o -isrc ${hugs-favourite}


##############################################################################
#
# Run test cases for both GHCI and Hugs
#

test:
	make test-ghc
	make test-hugs

test-ghc:
#
# The favoured GHC model
#
	${ghci} ${ghc-favourite} -v0 < Main.in > MainGhcGeneric1.out
	diff -b MainGhcGeneric1.out MainGhcGeneric1.ref
#
# The GHC model with TTypeable-based type equality
#
	${ghci} MainGhcTTypeable.hs -v0 < Main.in > MainGhcTTypeable.out
	diff -b MainGhcTTypeable.out MainGhcTTypeable.ref
#
# Run test cases as posted on mailing lists
#
	${ghci} MainPatternMatch.hs -v0 < Main.in > MainPatternMatch.out
	diff -b MainPatternMatch.out MainPatternMatch.ref
	${ghci} MainPosting-040607.hs -v0 < Main.in > MainPosting-040607.out
	diff -b MainPosting-040607.out MainPosting-040607.ref
	${ghci} MainPosting-051106.hs -v0 < Main.in > MainPosting-051106.out
	diff -b MainPosting-051106.out MainPosting-051106.ref
	${ghci} HSequenceTest.hs -v0 < Main.in > HSequence.out
	diff -b HSequence.out HSequence.ref 
#
# Yet another generic type equality
#
	${ghci} MainGhcGeneric2.hs -v0 < Main.in > MainGhcGeneric2.out
	diff -b MainGhcGeneric2.out MainGhcGeneric2.ref
#
# Yet another generic type cast
#
	${ghci} MainGhcGeneric3.hs -v0 < Main.in > MainGhcGeneric3.out
	diff -b MainGhcGeneric3.out MainGhcGeneric3.ref

test-hugs:
#
# The Hugs model with TTypeable-based type equality
#
	runhugs -98 +o ${hugs-favourite} < Main.in > MainHugsTTypeable.out
	diff -b MainHugsTTypeable.out MainHugsTTypeable.ref


##############################################################################
#
# Approve generated output as test results
#

copy:
	cp MainGhcGeneric1.out MainGhcGeneric1.ref
	cp MainGhcTTypeable.out MainGhcTTypeable.ref
	cp MainHugsTTypeable.out MainHugsTTypeable.ref
	cp MainPosting-040607.out MainPosting-040607.ref
	cp MainGhcGeneric2.out MainGhcGeneric2.ref
	cp MainGhcGeneric3.out MainGhcGeneric3.ref


##############################################################################
#
# Precompilation of HList.
#
# BEWARE!!!
# This may not work even if interpretation works.
# Depending on versions and platforms.
# Here is one scenario that leads to crashes:
#  - Tested under GHC 6.4 and Windows XP
#  - Compile CommonMain.hs *without* -O
#  - Run test cases with "make test"
#  - Runtime crashes in the middle of printing main's output.
#

CommonMain.o: *.hs Makefile
	rm -f *.o
	ghc  \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-c -O \
		--make \
		CommonMain.hs

Main.exe: *.hs Makefile
	rm -f *.o
	ghc  \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-o Main.exe -O \
		--make \
		Main.hs


##############################################################################
#
# Clean up directory
#

clean:
	rm -f *~
	rm -f *.out
	rm -f *.o
	rm -f *.exe
	rm -f *.hi
	rm -f index.html HList.zip


##############################################################################
#
# Target used by the authors for distributing OOHaskell.
#

distr:
	cat pre.html README post.html > index.html
	rm -rf HList.zip
	rm -rf HList
	mkdir -p HList
	cp --preserve *.hs *.lhs Makefile Main.in *.ref README LICENSE ChangeLog HList
	zip -r HList.zip HList


##############################################################################

commit:
	darcs record -a -m "Committed from the Makefile"
	darcs push -a

