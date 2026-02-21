Generated =
Generated += test/measured_narrow.txt
Generated += test/measured_wide.txt
Generated += UTF-8-EAW-CUSTOM
Generated += UTF-8-EAW-CUSTOM.gz
Generated += eaw-custom.el
Generated += eaw-custom-wezterm.lua

all:
	@./locale-gen.py --help
$(Generated) : src/ucd/UTF-8
src/ucd/UTF-8:
	make -C src/ucd

clean:
	rm -f $(Generated) *.el *.elc

distclean:
	make distclean -C src/ucd
