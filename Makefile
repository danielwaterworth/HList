##############################################################################

all:
	@echo "No default target for make."

##############################################################################

# Start a GHCI session for the favoured GHC model
ghci:
	ghci GhcGeneric.hs

##############################################################################

# Start a Hugs session for the favoured Hugs model
hugs:
	hugs -98 +o HugsTTypeable.hs

##############################################################################

# Run test cases for both GHCI and Hugs
test:
#
# The favoured GHC model
#
	ghci GhcGeneric.hs -v0 < Main.in > Main.out.GhcGeneric
	diff Main.out.GhcGeneric Main.ref.GhcGeneric
#
# The GHC model with TTypeable-based type equality
#
	ghci GhcTTypeable.hs -v0 < Main.in > Main.out.GhcTTypeable
	diff Main.out.GhcTTypeable Main.ref.GhcTTypeable
#
# The Hugs model with TTypeable-based type equality
#
	runhugs -98 +o HugsTTypeable.hs < Main.in > Main.out.HugsTTypeable
	diff Main.out.HugsTTypeable Main.ref.HugsTTypeable
#
# Run a test case as posed on Glasgow Haskell Users
#
	ghci gh-users-040607.hs -v0 < Main.in > Main.out.gh-users-040607
	diff Main.out.gh-users-040607 Main.ref.gh-users-040607

##############################################################################

# Approve generated output as test results
copy:
	cp Main.out.GhcGeneric Main.ref.GhcGeneric
	cp Main.out.GhcTTypeable Main.ref.GhcTTypeable
	cp Main.out.HugsTTypeable Main.ref.HugsTTypeable
	cp Main.out.gh-users-040607 Main.ref.gh-users-040607

##############################################################################

# Clean up directory
clean:
	rm -f *~
	rm -f *.out.*

##############################################################################
