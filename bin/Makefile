.PHONY: install busybox system clean

ifeq (0, $(shell id -u))
    dest := /usr/local/bin
else
    dest := $(HOME)/bin
endif

busybox := $(shell command -v busybox)

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
	cd ./system; find -type f,l -executable | rsync -lpt --files-from=- . $(dest)
	diff -rq ./system $(dest) || true
endif

clean:
	rm -rf $(dest)/busybox
ifneq ($(CURDIR), $(dest))
	find ./system -type f,l -executable -execdir rm -f $(dest)/{} \;
	ls -la $(dest)
endif
