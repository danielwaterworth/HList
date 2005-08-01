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

ghci = ghci -fglasgow-exts \
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
	hugs -98 +o ${hugs-favourite}


##############################################################################
#
# Run test cases for both GHCI and Hugs
#

test:

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
# The Hugs model with TTypeable-based type equality
#
	runhugs -98 +o ${hugs-favourite} < Main.in > MainHugsTTypeable.out
	diff -b MainHugsTTypeable.out MainHugsTTypeable.ref
#
# Run a test case as posed on Glasgow Haskell Users
#
	${ghci} MainPosting-040607.hs -v0 < Main.in > MainPosting-040607.out
	diff -b MainPosting-040607.out MainPosting-040607.ref
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
# Clean up directory
#

clean:
	rm -f *~
	rm -f *.out
	rm -f *.o
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
	cp --preserve *.hs *.lhs Makefile Main.in *.ref README LICENSE HList
	zip -r HList.zip HList


##############################################################################
