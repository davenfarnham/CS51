all: huffman zip unzip freq testing

huffman:
	corebuild huffman.native

zip:
	ocamlbuild -use-ocamlfind -tag thread -pkg core,batteries zip.native

unzip:
	ocamlbuild -use-ocamlfind -tag thread -pkg core,batteries unzip.native

freq:
	ocamlbuild -use-ocamlfind -tag thread -pkg core,batteries freq.native

testing:
	ocamlbuild -use-ocamlfind -tag thread -pkg core,batteries testing.native

clean:
	rm -rf _build *.native *.ml~ texts/*.zip
