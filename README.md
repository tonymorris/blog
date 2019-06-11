# thegapchessclub.org.au

### Making website changes

* Edit files ending in `.html` or `.md`
* These files can be found in:
  * the base directory
  * the `people` directory
  * the `snippets` directory
  * the `templates` directory
  * the `posts` directory
* Blog posts should go in the `posts` directory.
* To upload any other file to the website, put it in the `share` directory.
  The file will then be available at `https://thegapchessclub.org.au/share/<your-file>`
* After making changes, about 6 minutes later the changes should be live on the website.

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
