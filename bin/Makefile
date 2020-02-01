.PHONY: install busybox system clean

ifeq (0, $(shell id -u))
    dest := /usr/local/bin
else
    dest := $(HOME)/bin
endif

busybox := $(shell which busybox)

# ----------------------------------------

install: busybox system

busybox:
	rm -rf $(dest)/busybox
ifneq (, $(busybox))
	mkdir $(dest)/busybox
	$(busybox) --install -s $(dest)/busybox
endif

system:
ifneq ($(CURDIR), $(dest))
	find ./system -maxdepth 1 -executable ! -type d -execdir \
		cp -a --no-preserve=ownership {} $(dest) \;
	diff -rq ./system $(dest) || true
endif

clean:
	rm -rf $(dest)/busybox
ifneq ($(CURDIR), $(dest))
	find ./system -maxdepth 1 -executable ! -type d -execdir \
		rm -f $(dest)/{} \;
	ls -la $(dest)
endif
