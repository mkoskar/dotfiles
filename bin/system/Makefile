.PHONY: install

target = /usr/local/bin

install:
	find -maxdepth 1 -executable ! -type d -exec \
		cp -a --no-preserve=ownership '{}' $(target) \;
	diff -rq . $(target) || true
