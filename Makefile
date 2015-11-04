default:
	STACK_YAML=stack-client.yaml stack build
	STACK_YAML=stack-server.yaml stack build

	mkdir -p install
	cp -r .stack-work/install/x86_64-linux/lts-3.10/ghcjs-0.2.0.20151001_ghc-7.10.2/bin/ghcjs-test-client.jsexe install
	cp -r .stack-work/install/x86_64-linux/nightly-2015-11-03/7.10.2/bin/ghcjs-test-server install

hpack:
	(cd client && hpack)
	(cd server && hpack)
