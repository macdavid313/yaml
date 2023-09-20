##
# yaml -  a YAML processor for Allegro CL
#
# @file
# @version 0.1

LIBYAML_VERSION=0.2.5
LISP=

vendor:
	mkdir -p vendor
	curl -L https://github.com/yaml/libyaml/releases/download/$(LIBYAML_VERSION)/yaml-$(LIBYAML_VERSION).tar.gz | tar xz -C vendor

libyaml.%: vendor
	mkdir -p vendor/build
	@cd vendor/yaml-$(LIBYAML_VERSION)/; \
		./configure --prefix=$(shell pwd)/vendor/build --enable-static=no; \
		make; \
		make install
	cp -L vendor/build/lib/$@ ./$@

yaml.fasl:
	@$(LISP) -q -L pkg.cl -e '(yaml.pkg:build)' --kill

.PHONY: test
test: yaml.fasl
	$(LISP) -q -L yaml.fasl -L t/yaml.cl -e '(yaml.test:main)' --kill

clean:
	rm -rf ./vendor
	find . -type f -name "*.fasl" -delete
	find . -type f -name "*.so" -delete
	find . -type f -name "*.dll" -delete
	find . -type f -name "*.dylib" -delete

# end
