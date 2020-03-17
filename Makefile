EMACS ?= emacs
for_compile := init.el

all: init.elc

init.elc: no-load-path.el

%.elc: %.el
	echo "[compile] $$file" ;\
	$(EMACS) --debug-init -Q --batch -L . -f batch-byte-compile $<
