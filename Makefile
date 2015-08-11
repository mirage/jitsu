PACKAGES=-package lwt.syntax,lwt,dns.lwt,libvirt,cmdliner,ezxmlm,ipaddr,str,conduit,conduit.lwt-unix,xen-api-client,xen-api-client.lwt,irmin.unix,xenstore,xenstore_transport,xenstore_transport.lwt,uuidm,xenlight,xenlight.xentoollog
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=vm_stop_mode.ml vm_state.ml backends.mli options.ml rump.ml libvirt_backend.ml xapi_backend.ml libxl_backend.ml irmin_backend.mli dns_helpers.ml irmin_backend.ml synjitsu.mli synjitsu.ml jitsu.mli jitsu.ml main.ml
PWD=$(shell pwd)
SRC=$(PWD)/src
BIN=$(PWD)/bin
INSTALLDIR=/usr/local/bin

all: $(BIN)/jitsu

$(BIN)/jitsu: $(addprefix $(SRC)/, $(FILES))
	mkdir -p $(BIN)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o $(BIN)/jitsu -syntax camlp4o

install: $(BIN)/jitsu
	@echo "Installing jitsu in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(FILES)))
	cd $(SRC) ; rm -f $(addsuffix .cmx,$(basename $(FILES)))
	cd $(SRC) ; rm -f $(addsuffix .o,$(basename $(FILES)))
	cd $(SRC) ; rm -f jitsu *~ tags
	cd $(BIN) ; rm -f jitsu
