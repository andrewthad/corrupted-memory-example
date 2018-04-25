TEST_HC = ghc
OPT = 2

.PHONY: Main
Main:
	$(TEST_HC) $(shell find . -iname '*.hs') -fforce-recomp -o $@ -O$(OPT)

deps:
	cabal install containers ghc-prim primitive --with-ghc=$(TEST_HC) --allow-newer
