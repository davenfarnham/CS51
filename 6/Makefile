all: ps6

FILES=music.ml refs.ml streams.ml

ps6: $(FILES)
	@echo "Compiling..."
	corebuild refs.native
	corebuild streams.native
	corebuild music.native

check: $(FILES)
	chmod u+x ../check_width
	../check_width refs.ml
	../check_width streams.ml
	../check_width music.ml

clean:
	rm -rf _build *.native
