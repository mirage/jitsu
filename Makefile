.PHONY=all detect_backends test install clean

INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44

PWD=$(shell pwd)
SRC=$(PWD)/src
BIN=$(PWD)/bin
INSTALLDIR=/usr/local/bin
VERSION=$(shell cat VERSION)
VERSION_ML=$(SRC)/jitsu_version.ml

MAIN_FILE=main.ml
MAIN_FILE_PREFIX=$(addprefix $(SRC)/,$(MAIN_FILE))

# Available backends detected by configure
HAS_LIBVIRT=$(shell ocamlfind query libvirt && echo 1 || echo 0)
HAS_XAPI=$(shell ocamlfind query xen-api-client && echo 1 || echo 0)
HAS_XL=$(shell ocamlfind query xenctrl && echo 1 || echo 0)

# Configure backends
ifeq ($(HAS_LIBVIRT), 1) 
	BACKENDS+=libvirt:
	BACKEND_FILES += libvirt_backend.ml
	BACKEND_PKG := $(strip $(BACKEND_PKG),libvirt)
endif
ifeq ($(HAS_XAPI), 1) 
	BACKENDS+=xen-api-client:
	BACKEND_FILES += xapi_backend.ml
	BACKEND_PKG := $(strip $(BACKEND_PKG),xen-api-client,xen-api-client.lwt)
endif
ifeq ($(HAS_XL), 1) 
	BACKENDS+=xenctrl:
	BACKEND_FILES += libxl_backend.ml
	BACKEND_PKG := $(strip $(BACKEND_PKG),xenlight,xentoollog)
endif

BASE_FILES=vm_stop_mode.ml vm_state.ml xenstore.ml backends.mli vm_backends.ml options.ml rumprun.ml $(BACKEND_FILES) dns_helpers.ml irmin_backend.ml synjitsu.mli synjitsu.ml jitsu.mli jitsu.ml
BASE_PKG=-package lwt.syntax,lwt,dns.lwt,cmdliner,ezxmlm,ipaddr,str,conduit,conduit.lwt-unix,irmin.unix,xenstore,xenstore_transport,xenstore_transport.lwt,uuidm$(BACKEND_PKG)
BASE_FILES_PREFIX=$(addprefix $(SRC)/,$(BASE_FILES))

TEST_SRC=$(PWD)/lib_test
TEST_FILES=test_options.ml
TEST_FILES_PREFIX=$(addprefix $(TEST_SRC)/,$(TEST_FILES))
TEST_MAIN_FILE=test.ml
TEST_MAIN_FILE_PREFIX=$(addprefix $(TEST_SRC)/,$(TEST_MAIN_FILE))
TEST_PKG=$(BASE_PKG),alcotest
TEST_INCLUDE=-I $(SRC)

all: $(BIN)/jitsu

$(VERSION_ML): VERSION
	@echo 'let current="$(VERSION)"' > $(VERSION_ML)
	@echo 'Version $(VERSION)'

$(BIN)/jitsu: $(BASE_FILES_PREFIX) $(MAIN_FILE_PREFIX) $(VERSION_ML) 
	@[ "$(BACKEND_PKG)" == "" ] && \
		echo "Warning: No VM backends found. Install xenctrl, xen-api-client or libvirt with opam to add a backend." || \
		echo 'Detected backends: $(subst :, ,$(BACKENDS))'
	mkdir -p $(BIN)
	cd $(SRC) ; ocamlfind $(OCAMLOPT) $(INCLUDE) $(BASE_PKG) $(OPT) $(VERSION_ML) $(BASE_FILES) $(MAIN_FILE) -o $(BIN)/jitsu -syntax camlp4o

$(BIN)/test: $(BIN)/jitsu $(TEST_FILES_PREFIX) $(TEST_MAIN_FILE_PREFIX)
	cd $(TEST_SRC) ; ocamlfind $(OCAMLOPT) $(TEST_INCLUDE) $(TEST_PKG) $(OPT) $(BASE_FILES_PREFIX) $(TEST_FILES) $(TEST_MAIN_FILE) -o $(BIN)/test -syntax camlp4o

test: $(BIN)/test
	@echo "Running tests..."
	$(BIN)/test

install: $(BIN)/jitsu
	@echo "Installing jitsu $(VERSION) in $(INSTALLDIR)..."
	install -s $(BIN)/jitsu $(INSTALLDIR)/jitsu

clean:
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(BASE_FILES))) $(addsuffix .cmx,$(basename $(BASE_FILES))) $(addsuffix .o,$(basename $(BASE_FILES)))
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(MAIN_FILE))) $(addsuffix .cmx,$(basename $(MAIN_FILE))) $(addsuffix .o,$(basename $(MAIN_FILE)))
	cd $(SRC) ; rm -f jitsu *~ tags
	cd $(SRC) ; rm -f $(addsuffix .cmi,$(basename $(VERSION_ML))) $(addsuffix .cmx,$(basename $(VERSION_ML))) $(addsuffix .o,$(basename $(VERSION_ML)))
	cd $(BIN) ; rm -f jitsu test
	cd $(TEST_SRC) ; rm -f $(addsuffix .cmi,$(basename $(TEST_FILES))) $(addsuffix .cmx,$(basename $(TEST_FILES))) $(addsuffix .o,$(basename $(TEST_FILES)))
	cd $(TEST_SRC) ; rm -f $(addsuffix .cmi,$(basename $(TEST_MAIN_FILE))) $(addsuffix .cmx,$(basename $(TEST_MAIN_FILE))) $(addsuffix .o,$(basename $(TEST_MAIN_FILE)))
	rm -rf _tests
