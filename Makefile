all:
	cabal sandbox init
	cabal-meta install

install:
	cp .cabal-sandbox/bin/ztail-enqueue /usr/local/bin/ztail-enqueue
