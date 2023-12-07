##
# yaml -  a YAML processor for Allegro CL
#
# @file
# @version 0.1

yaml.fasl:
	$(LISP) -q -L pkg.cl -e '(yaml.pkg:build)' --kill

.PHONY: test
test: yaml.fasl
	$(LISP) -q -L pkg.cl -e '(yaml.pkg:test)' \
		-e '(yaml::test-lexer)' \
		--kill

clean:
	find . -type f -name "*.fasl" -delete
