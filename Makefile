default: communicate

communicate:
	ghc -prof -auto-all -odir out -hidir out -isrc -i/src Main