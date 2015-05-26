0.2
- Add --synjitsu command line parameter
- Support for optionally updating synjitsu with the MAC address of newly booted unikernels over vchan
- MAC address of each VM read from backend and stored in vm_metadata
- IP address added to vm_metadata
- `Jitsu.create` now has two optional arguments: vm_count and use_synjitsu
- libvirt-dev, libvirt-bin added as depext in Debian and Ubuntu
- conduit and vchan added as opam dependencies
- Removed version number from opam file in dev repo
- Destroyed domains can now be restarted (fixes #8)
- Update to ocaml-dns 0.15.0

0.1 
- First public release
