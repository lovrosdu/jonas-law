all: libtree-sitter-jonas-law.so

src/parser.c: grammar.js
	tree-sitter generate

libtree-sitter-jonas-law.so: CFLAGS=-shared -fPIC -fno-exceptions -g -O2 -I src -o $@
libtree-sitter-jonas-law.so: src/parser.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) -r src libtree-sitter-jonas-law.so
