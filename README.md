# blog.tmorris.net

### Build and run locally

##### cabal/nix

* `nix-shell -p 'haskell.packages.ghc822.ghcWithPackages (self: with self; [ zlib ])'`
* `cabal new-build`
* `./dist-newstyle/build/*/ghc-*/blog-*/c/site/build/site/site build`
* `./dist-newstyle/build/*/ghc-*/blog-*/c/site/build/site/site watch --no-server`
* `(cd public && python -m SimpleHTTPServer)`

##### stack

* `stack install --only-dependencies`
* `export STACK_ROOT=`pwd`/.stack` 
* `stack setup`
* `stack build`
* `stack exec site watch`
