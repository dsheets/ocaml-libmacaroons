all: _build/lib/dlllibmacaroons.so lib/macaroons_probe.ml
	ocamlbuild -use-ocamlfind lib/libmacaroons.cma lib/libmacaroons.cmxa

clean:
	ocamlbuild -use-ocamlfind -clean

test:
	ocamlbuild -use-ocamlfind lib_test/test_libmacaroons.native --

install: all
	ocamlfind install libmacaroons lib/META \
		$(addprefix _build/,lib/*.cmi lib/*.cma lib/*.cmxa lib/*.a lib/*.so)

uninstall:
	ocamlfind remove libmacaroons

reinstall: uninstall install

.PHONY: all clean test install uninstall reinstall

_build/lib/dlllibmacaroons.so:
	mkdir -p $$(dirname $@)
	$(CC) -shared -lmacaroons -o $@

_build/%: %.c
	mkdir -p $$(dirname $@)
	$(CC) -Wall -g -lmacaroons -o $@ $^

lib/macaroons_probe.ml: lib/macaroons_probe_gen.c
	gcc -Wall -o _build/macaroons_probe_gen lib/macaroons_probe_gen.c
	_build/macaroons_probe_gen
	mv macaroons_probe.ml lib/
