This is a simple MovieStore example written in Haskell which I am using for my
advanced functional programming lecture.

It contains an executable "moviestore" which can be build and installed via

    $ cabal sandbox init
    $ cabal install

and run via

    $ cabal run

It contains also some examples of haddock source code documentation which can be generated via

    $ cabal haddock --executables

and viewed using a web browser pointing to
<dist/doc/html/moviestore/moviestore/index.html>

It also contains some tests using QuickCheck2, HUnit and test-framework. To run these test do:

    $ cabal install --enable-tests
    $ cabal test

Furthermore, you can check the test coverage using

    $ hpc markup --exclude Main --destdir=dist/hpc moviestore-tests

and view the results with your browser pointing to
<dist/hpc/hpc_index.html>

Happy Haskell Hacking
