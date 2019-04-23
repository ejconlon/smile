.PHONY: build
build:
	stack build

.PHONY: splices
splices:
	stack build --ghc-options='-ddump-splices'
	find .stack-work -iname "*.dump-splices" | xargs -t -L1 cat

.PHONY: test
test:
	stack test

.PHONY: deps
deps:
	stack install apply-refact hlint intero stylish-haskell ghcid

.PHONY: deps
format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

.PHONY: lint
lint:
	stack exec hlint -- -i 'Parse error' -i 'Reduce duplication' src

.PHONY: refactor
refactor:
	find . -path ./.stack-work -prune -o -name '*.hs' | xargs -t -L1 stack exec hlint -- --refactor --refactor-options -i
