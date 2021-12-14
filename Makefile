MAKESAMPLEMAP = $(shell nix-store -r $(shell nix-instantiate webdirt.nix))/makeSampleMap.sh

all: build

build:
	nix-build -o result -A ghcjs.seis8s
	cp -Rf result/bin/seis8s.jsexe .
	cp -f style.css seis8s.jsexe/style.css

makeSampleMap:
	@ echo "makeSampleMap:"
	@[ -d static/samples ] || (echo Directory static/samples does not exist. Have you provided a sample library, for example, by running 'make downloadDirtSamples'? && exit 1)
	cd static/samples && bash $(MAKESAMPLEMAP) . > sampleMap.json
	@[ -f static/samples/sampleMap.json ] || (echo "Error: make makeSampleMap did NOT work!" && exit 1)
	@ echo "Sample map made."

devTest:
	cabal --ghcjs new-test test:tests --disable-library-profiling --disable-documentation

ghcBuild:
	cabal --builddir=ghc-result new-build all --disable-library-profiling --disable-documentation


bundleClient:
	zip -r - ./seis8s.jsexe/* > seis8s-standalone.zip

serve:
	cd seis8s.jsexe; python3 -m http.server

clean:
	rm -rf seis8s.jsexe
	rm -rf result
	rm -rf dev-result
