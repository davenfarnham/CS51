all: modules ps4
	corebuild modules.native
	corebuild ps4.native

modules:
	corebuild modules.native

ps4:
	corebuild ps4.native

check:
	chmod u+x ../check_width
	../check_width modules.ml
	../check_width ps4.ml

clean:
	rm -rf _build *.native
