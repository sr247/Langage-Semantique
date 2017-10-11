.PHONY:	 all clean

OCB_FLAGS = -cflag -w -cflag -8 -use-ocamlfind -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	rm -RIvfd --one-file-system --preserve-root _build/ VM

native:
	$(OCB) Main.native
	mv Main.native VM
