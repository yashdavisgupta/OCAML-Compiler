slangc: slangc.ml
	ocamlc slangc.ml -o slangc

clean:
	rm -f *.cm[ioxa] *.cmxa *.a *.o sample *~ test

test:
	sh script.sh
