EMACS ?= emacs
for_compile := init.el
VERSION := $(shell git rev-parse --short HEAD)
TARBALL := out/100ms_dot_emacs.v$(VERSION).tar.gz
SRC_TARBALL := out/100ms_dot_emacs.src.v$(VERSION).tar.gz
SRC_ZIPBALL := out/100ms_dot_emacs.src.v$(VERSION).zip

## ---------------------------------------------------
all: $(SRC_TARBALL) $(SRC_ZIPBALL)  $(TARBALL) 

$(TARBALL): $(HOME)/.emacs.d/init.elc
	(cd $(HOME); tar -zcf - .emacs.d) >$@

$(SRC_TARBALL): 
	git archive --format=tar.gz -o $@ --prefix=100ms_dot_emacs.v$(VERSION)/ HEAD
$(SRC_ZIPBALL): 
	git archive --format=zip -o $@ --prefix=100ms_dot_emacs.v$(VERSION)/ HEAD

$(HOME)/.emacs.d/init.elc:  init.elc
	cp -v $< $@

init.elc: no-load-path.el

%.elc: %.el
	echo "[compile] $<" ;\
	$(EMACS) --debug-init -Q --batch -L . -f batch-byte-compile $<

clean:
	rm out/*.tar.gz out/*.zip $(HOME)/.emacs.d/init.elc
