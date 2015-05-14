BYTECOMPILES = neopastebin.elc

.PHONY: all clean
all: $(BYTECOMPILES)

%.elc: %.el
	emacs --batch --eval '(byte-compile-file "$<")'

clean:
	rm -rf $(BYTECOMPILES)
