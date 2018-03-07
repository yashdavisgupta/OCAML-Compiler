
OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top

all: slangc.native
debug: all slangc.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean

top: slangc.cma
	utop

test:
	sh script.sh
