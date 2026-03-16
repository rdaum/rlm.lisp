LEM_DIR ?= $(HOME)/lem
LISP ?= sbcl --noinform --no-sysinit --no-userinit

test:
	cd $(LEM_DIR) && $(LISP) \
		--load .qlot/setup.lisp \
		--eval '(pushnew (merge-pathnames "rlm/" (user-homedir-pathname)) asdf:*central-registry*)' \
		--eval '(asdf:test-system "rlm/mode-tests")' \
		--quit

.PHONY: test
