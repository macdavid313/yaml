##
# yaml -  a YAML processor for Allegro CL
#
# @file
# @version 0.1

LIBYAML_VERSION=0.2.5
LISP=

.PHONY: libyaml_src
libyaml_src:
	@if [ -d yaml-$(LIBYAML_VERSION) ]; then \
		echo yaml-$(LIBYAML_VERSION) already exists; \
	else \
		curl -L https://github.com/yaml/libyaml/releases/download/$(LIBYAML_VERSION)/yaml-$(LIBYAML_VERSION).tar.gz | tar xz; \
	fi

libyaml.%: libyaml_src
	@cd yaml-$(LIBYAML_VERSION)/; \
		mkdir -p build/; \
		./configure --prefix=$(shell pwd)/yaml-$(LIBYAML_VERSION)/build --enable-static=no; \
		make; \
		make install
	cp -L yaml-$(LIBYAML_VERSION)/build/lib/$@ ./

yaml.fasl:
	@$(LISP) -q -L pkg.cl -e '(yaml.pkg:build-yaml)' --kill

clean:
	rm -rf yaml-$(LIBYAML_VERSION)
	find . -type f -name "*.fasl" -delete
	find . -type f -name "*.so" -delete
	find . -type f -name "*.dll" -delete
	find . -type f -name "*.dylib" -delete

# end
