UNICODE_VERSION=17.0.0
URI=https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd

all: UTF-8 emoji-data.txt i18n_ctype 

UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt:
	test -f $@ || curl -L -O $(URI)/$@

emoji-data.txt:
	test -f $@ || curl -L -O $(URI)/emoji/$@

unicode_utils.py:
	test -f $@ || curl -L -O $@ https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/$@ -o $@

utf8_gen.py: unicode_utils.py
	test -f $@ || curl -L -O $@ https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/$@ -o $@
	chmod u+x utf8_gen.py

gen_unicode_ctype.py:
	test -f $@ || curl -L -O https://raw.githubusercontent.com/bminor/glibc/master/localedata/unicode-gen/$@
	chmod u+x gen_unicode_ctype.py

i18n_ctype: gen_unicode_ctype.py UnicodeData.txt DerivedCoreProperties.txt
	python3 gen_unicode_ctype.py -u UnicodeData.txt \
	  -d DerivedCoreProperties.txt -o $@ \
	  --unicode_version $(UNICODE_VERSION)

UTF-8: UnicodeData.txt EastAsianWidth.txt DerivedCoreProperties.txt HangulSyllableType.txt
	./utf8_gen.py \
		-u UnicodeData.txt \
		-d DerivedCoreProperties.txt \
		-e EastAsianWidth.txt \
		-k HangulSyllableType.txt \
		--unicode_version $(UNICODE_VERSION)
clean:
	rm -rf UTF-8 i18n_ctype *.txt emoji-data.txt

distclean: clean
	rm -fr __pycache__
