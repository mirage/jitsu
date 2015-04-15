PACKAGES=-package str,lwt,dns.lwt,libvirt,cmdliner,ezxmlm,ipaddr
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=jitsu.mli jitsu.ml main.ml
PWD=$(shell pwd)
SRC=$(PWD)/src
BIN=$(PWD)/bin
INSTALLDIR=/usr/local/bin

all: $(BIN)/jitsu

$(BIN)/jitsu: $(SRC)/jitsu.ml $(SRC)/main.ml
	mkdir -p $(BIN)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/jitsu

install: $(BIN)/jitsu
	@echo "Installing jitsu in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f jitsu jitsu.cmx jitsu.cmi jitsu.o main.o main.cmx main.cmi
	cd $(SRC) ; rm -f *~ tags
	cd $(BIN) ; rm -f jitsu
