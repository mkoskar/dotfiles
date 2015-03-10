.PHONY: all

all:
	@for i in linux* rxvt* screen* xterm*; do \
		echo "-> $$i"; \
		tic -o /etc/terminfo $$i; \
	done
