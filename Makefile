##############################################################################

all:
	@echo "No default target for make."

##############################################################################
#
# Start a GHCI session for the favoured GHC model
#
ghci:
	ghci MainGhcGeneric.hs

##############################################################################
#
# Start a Hugs session for the favoured Hugs model
#
hugs:
	hugs -98 +o MainHugsTTypeable.hs

##############################################################################
#
# Run test cases for both GHCI and Hugs
#
test:
#
# The favoured GHC model
#
	ghci MainGhcGeneric.hs -v0 < Main.in > MainGhcGeneric.out
	diff MainGhcGeneric.out MainGhcGeneric.ref
#
# The GHC model with TTypeable-based type equality
#
	ghci MainGhcTTypeable.hs -v0 < Main.in > MainGhcTTypeable.out
	diff MainGhcTTypeable.out MainGhcTTypeable.ref
#
# The Hugs model with TTypeable-based type equality
#
	runhugs -98 +o MainHugsTTypeable.hs < Main.in > MainHugsTTypeable.out
	diff MainHugsTTypeable.out MainHugsTTypeable.ref
#
# Run a test case as posed on Glasgow Haskell Users
#
	ghci MainPosting-040607.hs -v0 < Main.in > MainPosting-040607.out
	diff MainPosting-040607.out MainPosting-040607.ref

##############################################################################
#
# Approve generated output as test results
#
copy:
	cp MainGhcGeneric.out MainGhcGeneric.ref
	cp MainGhcTTypeable.out MainGhcTTypeable.ref
	cp MainHugsTTypeable.out MainHugsTTypeable.ref
	cp MainPosting-040607.out MainPosting-040607.ref

##############################################################################
#
# Clean up directory
#
clean:
	rm -f *~
	rm -f *.out

##############################################################################
