all:
	cabal sandbox init
	cabal-meta install

install:
	cp .cabal-sandbox/bin/ztail /usr/local/bin/ztail
