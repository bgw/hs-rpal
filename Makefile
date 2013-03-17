# We're required to have a Makefile for COP5555, so this is just a stub of one.
# The actual build process is done with cabal.

all:
	@echo "'make all' is a no-op. The CISE servers don't have ghc and cabal."
	@echo "A static binary is included so that you can test the program"

cl:
	@echo "Again, this is a no-op because the CISE servers don't have haskell."
	@echo "If we deleted 'dist', you'd have no way of rebuilding."

run:
	@echo "This command effectively does nothing, because we exit on no input."
	@echo "Try running ./dist/build/hs-rpal-compat/hs-rpal-compat with args.\n"
	./dist/build/hs-rpal-compat/hs-rpal-compat
	
test:
	@echo "I followed the Project 1 example Makefile."
	@echo "This assumes 'rpal' and 'difftest.pl' are in the pwd and that"
	@echo "~/rpal/tests exists.\n"
	
	./difftest.pl \
		-1 "./rpal -ast -noout FILE" \
		-2 "./dist/build/hs-rpal-compat/hs-rpal-compat -ast -noout FILE" \
		-t ~/rpal/tests/
