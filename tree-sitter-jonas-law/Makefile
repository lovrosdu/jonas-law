GRAMMARS := jonas-law.js
DIRS := $(patsubst %.js, %, $(GRAMMARS))
LIBS := $(patsubst %.js, libtree-sitter-%.so, $(GRAMMARS))

all: $(LIBS)

$(LIBS): libtree-sitter-%.so: %.js
	tree-sitter generate --no-bindings "$<"
	$(RM) -rf "$*" && mv src "$*"
	$(CC) $(CPPFLAGS) $(CFLAGS) -shared -fPIC -fno-exceptions -g -O2 -I "$*" -o "$@" "$*/parser.c"

.PHONY: clean
clean:
	$(RM) -r $(DIRS) $(LIBS)
