all: deps
	cabal install

deps:
	cabal sandbox init
	mkdir -p dependencies
	bash -c "cd dependencies && if [ ! -d ZTail-Lib ] ; then (git clone https://github.com/adarqui/ZTail-Lib && cd ZTail-Lib && make); fi"
	cabal sandbox add-source dependencies/ZTail-Lib

install:
	cp .cabal-sandbox/bin/ztail-enqueue /usr/local/bin/ztail-enqueue-redis
	cp .cabal-sandbox/bin/ztail-dump /usr/local/bin/ztail-dump-redis
