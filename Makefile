EMACS ?= emacs
for_compile := init.el
VERSION := $(shell git describe --always --tags HEAD)
TARBALL := out/100ms_dot_emacs.emacs.d.$(VERSION).tar.gz

## ---------------------------------------------------
all: $(SRC_TARBALL) $(SRC_ZIPBALL)  $(TARBALL)

$(TARBALL): $(HOME)/.emacs.d/init.elc
	(cd -P $(HOME)/.emacs.d; cd ..;tar -zcf - \
		--exclude=.emacs.d/straight/repos \
		.emacs.d/.autoloads.el \
		.emacs.d/.autoloads.elc \
        .emacs.d/init.elc .emacs.d/straight) >$@; ln -sf `basename $@` out/100ms_dot_emacs.emacs.d.latest.tar.gz

$(HOME)/.emacs.d/init.elc:  init.elc
	cp -v $< $@

init.elc: no-load-path.el

%.elc: %.el
	echo "[compile] $<" ;\
	$(EMACS) -Q --batch -L . --eval="(require 'no-load-path)" -f batch-byte-compile $<

clean:
	-rm out/*.tar.gz  init.elc $(HOME)/.emacs.d/init.elc
