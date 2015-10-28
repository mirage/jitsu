PACKAGES=-package lwt.syntax,lwt,dns.lwt,libvirt,cmdliner,ezxmlm,ipaddr,str,conduit,conduit.lwt-unix,xen-api-client,xen-api-client.lwt,irmin.unix,xenstore,xenstore_transport,xenstore_transport.lwt,uuidm,xenlight,xentoollog
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44

SRC=$(PWD)/src
FILES=vm_stop_mode.ml vm_state.ml xenstore.ml backends.mli options.ml rumprun.ml libvirt_backend.ml xapi_backend.ml libxl_backend.ml dns_helpers.ml irmin_backend.ml synjitsu.mli synjitsu.ml jitsu.mli jitsu.ml
MAIN=main.ml

TEST_SRC=$(PWD)/lib_test
TEST_FILES=test_options.ml
TEST_MAIN=test.ml
TEST_PACKAGES=$(PACKAGES),alcotest
TEST_INCLUDE=-I $(SRC)

PWD=$(shell pwd)
BIN=$(PWD)/bin
INSTALLDIR=/usr/local/bin
VERSION=$(shell cat VERSION)
VERSION_ML=$(SRC)/jitsu_version.ml

all: $(BIN)/jitsu

$(VERSION_ML): VERSION
	echo 'let current="$(VERSION)"' > $(VERSION_ML)

$(BIN)/jitsu: $(addprefix $(SRC)/, $(FILES)) $(addprefix $(SRC)/, $(MAIN)) $(VERSION_ML)
	mkdir -p $(BIN)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(VERSION_ML) $(FILES) $(MAIN) -o $(BIN)/jitsu -syntax camlp4o

$(BIN)/test: $(BIN)/jitsu $(addprefix $(TEST_SRC)/, $(TEST_FILES)) $(addprefix $(TEST_SRC)/, $(TEST_MAIN))
	cd $(TEST_SRC) ; ocamlfind $(OCAMLOPT) $(TEST_INCLUDE) $(TEST_PACKAGES) $(OPT) $(addprefix $(SRC)/, $(FILES)) $(TEST_FILES) $(TEST_MAIN) -o $(BIN)/test -syntax camlp4o

test: $(BIN)/test
	@echo "Running tests..."
	$(BIN)/test

install: $(BIN)/jitsu
	@echo "Installing jitsu $(VERSION) in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(FILES))) $(addsuffix .cmx,$(basename $(FILES))) $(addsuffix .o,$(basename $(FILES)))
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(MAIN))) $(addsuffix .cmx,$(basename $(MAIN))) $(addsuffix .o,$(basename $(MAIN)))
	cd $(SRC) ; rm -f jitsu *~ tags
	cd $(SRC) ; rm -f $(VERSION_ML)
	cd $(BIN) ; rm -f jitsu test
	cd $(TEST_SRC) ; rm -f $(addsuffix .cmi,$(basename $(TEST_FILES))) $(addsuffix .cmx,$(basename $(TEST_FILES))) $(addsuffix .o,$(basename $(TEST_FILES)))
	cd $(TEST_SRC) ; rm -f $(addsuffix .cmi,$(basename $(TEST_MAIN))) $(addsuffix .cmx,$(basename $(TEST_MAIN))) $(addsuffix .o,$(basename $(TEST_MAIN)))
	rm -rf _tests
