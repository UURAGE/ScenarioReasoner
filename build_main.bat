set ghcFlags=--make -Wall -O2 -no-user-package-db -package-db "../.cabal-sandbox/x86_64-windows-ghc-7.8.4-packages.conf.d" -hidir "../intermediate" -odir "../intermediate" -stubdir "../intermediate"
mkdir intermediate
cd src
ghc %ghcFlags% -o ../Main.cgi "./Main.hs"
cd ../
pause