0.2.2 (2015-15-19)

- Support xenctrl > 0.9.26

0.2.1 (2015-15-19)

- Add/fix support for multiple DNS entries per VM
- Update storage backend to support latest Irmin API (0.10.0)
- Add VERSION file to track version in builds and releases
- Update README with links to paper and recent blog posts

0.2 (2015-17-08)

General:
- Add support for rumprun unikernels in libxl backend (rumprun_config option)
- New configuration syntax with key/value pairs
- VMs are now destroyed by default (was suspend, can be changed with -m)
- Support for waiting for a key in Xenstore before sending DNS reply (wait_for_key option)
- Support for setting fixed response delay per domain (response_delay option, overrides -d)
- Support more than one DNS name per VM

Storage:
- All storage functionality moved to separate storage module
- Irmin in-memory storage backend to store internal state
- Add --persistdb parameter to persist Irmin db to disk

Synjitsu:
- Add --synjitsu parameter to connect to Synjitsu
- Support for optionally updating synjitsu with the MAC address of newly booted unikernels
- Support for enabling/disabling synjitsu per domain (use_synjitsu option)

VM backends:
- All VM management moved to backend modules that implement backends.mli
- Libvirt support now in separate module
- Add experimental XAPI backend support (from Masoud Koleini @koleini)
- Add experimental libxl backend support (from Dave Scott @djs55)
- Add disk support in libxl backend
- Add support for multiple network interfaces and configuration scripts in libxl backend

Tests:
- Test framework based on alcotest added, but needs more tests
- Implemented tests for options.ml

Bugfixes:
- Destroyed domains can now be restarted (fixes #8)

Packaging:
- Removed version number from opam file in dev repo
- libvirt-dev, libvirt-bin added as depext in Debian and Ubuntu
- conduit, vchan, xen-api-client added as opam dependencies
- Update to ocaml-dns 0.15.3
- Enable Travis on Github repository 

0.1 
- First public release
