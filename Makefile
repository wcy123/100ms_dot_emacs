EMACS ?= emacs
for_compile := init.el

all: $(HOME)/.emacs.d/init.elc

$(HOME)/.emacs.d/init.elc:  init.elc
	cp -v $< $@

init.elc: no-load-path.el

%.elc: %.el
	echo "[compile] $<" ;\
	$(EMACS) --debug-init -Q --batch -L . -f batch-byte-compile $<
