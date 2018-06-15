################################################################################
# Builds
################################################################################
.PHONY: configure # Used for building caches
configure:
	nix-shell --run "cabal new-configure" --argstr compiler ghc822

.PHONY: build
build:
	nix-shell --run "cabal new-build -v" --argstr compiler ghc822

# Mimmicks production hab settings
.PHONY: build-prod
build-prod:
	nix-shell --run "cabal new-build -v  --ghc-options='-Werror'" --argstr compiler ghc822

.PHONY: audit
audit:
	nix-shell --run "cabal new-build -v --ghc-options='-Werror -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints -v0' -v0" --argstr compiler ghc822

.PHONY: test
test:
	nix-shell --run "cabal new-test" --argstr compiler ghc822

.PHONY: test-prod
test-prod:
	nix-shell --run "cabal new-test" --argstr compiler ghc822

.PHONY: clean
clean:
	rm -rf dist
	rm -rf dist-newstyle

# Nix

.PHONY: nix-gen
nix-gen:
	cabal2nix . > packages.nix
