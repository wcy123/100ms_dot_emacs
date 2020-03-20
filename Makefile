EMACS ?= emacs
for_compile := init.el
VERSION := $(shell git describe --tags HEAD)
TARBALL := out/100ms_dot_emacs.emacs.d.$(VERSION).tar.gz

## ---------------------------------------------------
all: $(SRC_TARBALL) $(SRC_ZIPBALL)  $(TARBALL)

$(TARBALL): $(HOME)/.emacs.d/init.elc
	(cd $(HOME); tar -zcf - --exclude=.emacs.d/straight/repos .emacs.d) >$@

$(HOME)/.emacs.d/init.elc:  init.elc
	cp -v $< $@

init.elc: no-load-path.el

%.elc: %.el
	echo "[compile] $<" ;\
	$(EMACS) -Q --batch -L . --eval="(require 'no-load-path)" --eval="(no-load-path-initialize)" -f batch-byte-compile $<

clean:
	rm out/*.tar.gz out/*.zip $(HOME)/.emacs.d/init.elc
