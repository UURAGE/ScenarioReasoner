cabal update
cabal install wl-pprint
cabal install SHA
cabal install uniplate
cabal install filemanip
cabal install syb
cabal install Diff
cabal install HDBC
REM on error of cabal install HDBC-sqlite3 download sqlite3 dll and header file and add --extra-include-dirs=<sqlite3.h directory> --extra-lib-dirs=<sqlite3.dll directory>
cabal install HDBC-sqlite3 
cabal install exceptions
cabal install QuickCheck-2.6
cabal install containers
cabal install random
cabal install time
cabal install filepath
cabal install parsec
cabal install directory
cabal install binary
cabal install xhtml
cabal install old-locale
cabal install network
cabal install mtl == 2.1.*
cabal install old-time
cabal install multipart
cabal install bytestring
cabal install array
cabal install transformers-0.3.0.0
cabal install network-uri
pause