##
# yaml -  a YAML processor for Allegro CL
#
# @file
# @version 0.1

LIBFYAML_VERSION=0.8
LISP=

vendor:
	mkdir -p vendor;
	curl -L https://github.com/pantoniou/libfyaml/releases/download/v${LIBFYAML_VERSION}/libfyaml-${LIBFYAML_VERSION}.tar.gz | tar xz -C vendor

libfyaml.%: vendor
	mkdir vendor/build
	@cd vendor/libfyaml-$(LIBFYAML_VERSION)/; \
		./configure --prefix=$(shell pwd)/vendor/build --enable-static=no --enable-debug=no; \
		make -j; \
		make install
	cp -L vendor/build/lib/$@ ./

yaml.fasl:
	@$(LISP) -q -L pkg.cl -e '(yaml.pkg:build)' --kill

clean:
	rm -rf vendor
	find . -type f -name "*.fasl" -delete
	find . -type f -name "*.so" -delete
	find . -type f -name "*.dll" -delete
	find . -type f -name "*.dylib" -delete

# end
