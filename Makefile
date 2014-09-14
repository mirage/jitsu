PACKAGES=-package str,lwt,dns.lwt,libvirt,cmdliner
INCLUDE=
OPT=-linkpkg -g 
OCAMLOPT=ocamlopt -w A-4-44
FILES=jitsu.mli jitsu.ml main.ml

all: jitsu

jitsu: jitsu.ml main.ml
	ocamlfind $(OCAMLOPT) $(INCLUDE) $(PACKAGES) $(OPT) $(FILES) -o jitsu

clean:
	rm -f jitsu jitsu.cmx jitsu.cmi jitsu.o main.o main.cmx main.cmi
	rm -f *~ tags
