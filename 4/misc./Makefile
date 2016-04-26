all: testing huffman trees code
	corebuild code.native
	corebuild trees.native
	corebuild huffman.native
	corebuild testing.native

code:
	corebuild code.native

trees:
	corebuild trees.native

huffman:
	corebuild huffman.native

compress:
	ocamlbuild -use-ocamlfind -tag thread -pkg core,batteries compress.native

testing:
	corebuild testing.native

clean:
	rm -rf _build *.native *.ml~
