all:
	@echo "No default target for make."

# Start a GHCI session for the favourite GHC model
ghci:
	ghci GhcGeneric.hs


# Start a Hugs session for the favourite Hugs model
hugs:
	hugs -98 +o HugsTTypeable.hs


# Run some test cases for both GHCI and Hugs
test:
	ghci GhcGeneric.hs -v0 < Main.in > Main.out.GhcGeneric
	diff Main.out.GhcGeneric Main.ref.GhcGeneric
	ghci GhcTTypeable.hs -v0 < Main.in > Main.out.GhcTTypeable
	diff Main.out.GhcTTypeable Main.ref.GhcTTypeable
	runhugs -98 +o HugsTTypeable.hs < Main.in > Main.out.HugsTTypeable
	diff Main.out.HugsTTypeable Main.ref.HugsTTypeable

# Approve generated output as test results
copy:
	cp Main.out.GhcGeneric Main.ref.GhcGeneric
	cp Main.out.GhcTTypeable Main.ref.GhcTTypeable
	cp Main.out.HugsTTypeable Main.ref.HugsTTypeable

# Clean up directory
clean:
	rm -f *~
	rm -f *.out.*
