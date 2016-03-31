all: moogle

# These must be in the right order--no forward refs
FILES = order.ml dict.ml myset.ml graph.ml nodescore.ml util.ml \
	query.ml pagerank.ml crawl.ml moogle.ml 

moogle: $(FILES)
	ocamlc -g -o moogle unix.cma str.cma $(FILES)

clean: 
	rm -f moogle *.cmi *.cmo
