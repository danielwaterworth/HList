##############################################################################

ghc-favourite  = MainGhcGeneric1.hs
hugs-favourite = MainHugsTTypeable.hs

##############################################################################

all:
	@echo "No default target for make."

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
	ghci ${ghc-favourite} -v0 < Main.in > MainGhcGeneric1.out
	diff MainGhcGeneric1.out MainGhcGeneric1.ref
#
# The GHC model with TTypeable-based type equality
#
	ghci MainGhcTTypeable.hs -v0 < Main.in > MainGhcTTypeable.out
	diff MainGhcTTypeable.out MainGhcTTypeable.ref
#
# The Hugs model with TTypeable-based type equality
#
	runhugs -98 +o ${hugs-favourite} < Main.in > MainHugsTTypeable.out
	diff MainHugsTTypeable.out MainHugsTTypeable.ref
#
# Run a test case as posed on Glasgow Haskell Users
#
	ghci MainPosting-040607.hs -v0 < Main.in > MainPosting-040607.out
	diff MainPosting-040607.out MainPosting-040607.ref
#
# Yet another generic type equality
#
	ghci MainGhcGeneric2.hs -v0 < Main.in > MainGhcGeneric2.out
	diff MainGhcGeneric2.out MainGhcGeneric2.ref


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

##############################################################################
#
# Clean up directory
#
clean:
	rm -f *~
	rm -f *.out

##############################################################################
