all:
	@echo "No default target for make."

# Start a GHCI session for the favourite GHC model
ghci:
	ghci Main1.hs


# Start a Hugs session for the favourite Hugs model
hugs:
	hugs -98 +o Main2.hs


# Run some test cases for both GHCI and Hugs
test:
	ghci Main1.hs -v0 < Main.in > Main.run.ghci
	diff Main.run.ghci Main.out.ghci
	ghci Main2.hs -v0 < Main.in > Main.run.ghci
	diff Main.run.ghci Main.out.ghci
	runhugs -98 +o Main2.hs < Main.in > Main.run.hugs
	diff Main.run.hugs Main.out.hugs


# Approve current session outputs as test data
copy:
	cp Main.run.ghci Main.out.ghci
	cp Main.run.hugs Main.out.hugs


# Clean up directory
clean:
	rm -f *~
	rm -f Main.run.*
