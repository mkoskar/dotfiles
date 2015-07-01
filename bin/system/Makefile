.PHONY: install

# Quick and dirty installation method.
# TODO: package this properly

install:
	@find -maxdepth 1 -executable ! -type d -exec \
		cp -a --no-preserve=ownership '{}' /usr/local/bin \;
