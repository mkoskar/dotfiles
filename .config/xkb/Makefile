.PHONY: Makefile build clean install

src := /usr/share/X11/xkb
dest := $(src)

custom_rules := rules/custom
evdev_rules := $(src)/rules/evdev

build: $(custom_rules) $(custom_rules).lst

clean:
	rm -f $(custom_rules){,.lst}

$(custom_rules): $(evdev_rules) $(custom_rules).in
	cat $+ >$@

$(custom_rules).lst: $(evdev_rules).lst $(custom_rules).lst.in
	cat $+ >$@

install: build
	rsync -rp -iv \
		-f '+ */' \
		-f '+ */custom' \
		-f '+ rules/custom.lst' \
		-f '- *' \
		./ $(dest)
