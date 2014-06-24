set outputfileA="../_output/ideas/"
set outputfileB="../_output/scriptman/"
set ghcFlags=--make -static -optl-static -O2 -H14m -hidir "../_output/intermediate" -odir "../_output/intermediate" -stubdir "../_output/intermediate"

if EXIST "./_output" rmdir /s/q "./_output"

mkdir "./_output"
cd "./_output"
mkdir ideas
mkdir scriptman
mkdir intermediate
cd ../
cd ./ScriptManServer
ghc %ghcFlags% -o ../_output/scriptman/Main.cgi "./Main.hs"
cd ../src
ghc %ghcFlags% -o ../_output/ideas/Main.cgi "./Main.hs"
cd ../
pause