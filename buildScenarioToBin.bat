set outputfile="../_output/ideas/"
set ghcFlags=--make -Wall -static -optl-static -O2 -H14m -hidir "../_output/intermediate" -odir "../_output/intermediate" -stubdir "../_output/intermediate"

mkdir "./_output"
cd "./_output"
mkdir ideas
mkdir intermediate
cd ../
cd src
ghc %ghcFlags% -o ../_output/ideas/scenarioToBin.cgi "./scenarioToBin.hs"
cd ../
pause