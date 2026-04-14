hadolint.wasm: app/hadolint.hs
	cabal \
		--with-compiler=wasm32-wasi-ghc \
		--with-hc-pkg=wasm32-wasi-ghc-pkg \
		--with-hsc2hs=wasm32-wasi-hsc2hs \
		--with-haddock=wasm32-wasi-haddock \
		build

ghc_wasm_jsffi.js: hadolint.wasm
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs -i hadolint.wasm -o js/ghc_wasm_jsffi.js
