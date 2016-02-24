all: expression mapfold test

mapfold: mapfold.ml
	corebuild mapfold.native

expression: $(FILES)
	corebuild expression.native

test: $(FILES)
	corebuild test.native

check: expression.ml  mapfold.ml
	chmod u+x ../check_width
	../check_width mapfold.ml
	../check_width expression.ml

clean:
	rm -rf _build *.native *.ml~
