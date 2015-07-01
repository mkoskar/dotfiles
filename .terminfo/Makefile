.PHONY: install

# Quick and dirty installation method.
# TODO: package this properly

install:
	@for i in linux* rxvt* screen* xterm*; do \
		echo "-> $$i"; \
		tic -o /etc/terminfo $$i; \
	done
