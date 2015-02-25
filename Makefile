PACKAGES=-package str,lwt,dns.lwt,xenlight,xenlight.xentoollog,cmdliner,uuidm,xenstore,xenstore.client,xenstore_transport,xenstore_transport.lwt
OPAM_DEPS=lwt dns xenctrl cmdliner uuidm xenstore xenstore_transport
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
PWD=$(shell pwd)
SRC=$(PWD)/src
BIN=$(PWD)/bin
FILES=jitsu.mli jitsu.ml main.ml
INSTALLDIR=/usr/local/bin

all: $(BIN)/jitsu

$(BIN)/jitsu: $(SRC)/jitsu.ml $(SRC)/main.ml
	mkdir -p $(BIN)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/jitsu

.PHONY: install-deps
install-deps:
	opam install $(OPAM_DEPS)

install: $(BIN)/jitsu
	@echo "Installing jitsu in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f jitsu jitsu.cmx jitsu.cmi jitsu.o main.o main.cmx main.cmi
	cd $(SRC) ; rm -f *~ tags
	cd $(BIN) ; rm -f jitsu
