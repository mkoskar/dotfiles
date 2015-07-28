.PHONY: install

install:
	# Quick and dirty installation method.
	@find -maxdepth 1 -executable ! -type d -exec \
		cp -a --no-preserve=ownership '{}' /usr/local/bin \;
