# Just-In-Time Summoning of Unikernels #

Jitsu is a forwarding DNS server that automatically starts virtual machines (VMs) on demand. When a DNS query is received, jitsu first checks for a local VM that is mapped to the requested domain. If a VM is found, the VM is started and its IP is returned to the client. Otherwise, the request is forwarded to the next DNS server. If no DNS requests are received for the VM within a given timeout period it is automatically stopped. 

Although Jitsu can be used with any VM that can be controlled with libvirt, it is mainly intended for use with [unikernels](http://www.openmirage.org) that can be started quickly and be able to respond to the client request within the time it takes to send the DNS response. 

## Getting started ##

Jitsu requires the libraries dns, lwt, libvirt and cmdliner, which can be installed with [opam](https://opam.ocaml.org). Run make to compile:

```
opam install dns lwt libvirt cmdliner
make
```

You should now be able to start jitsu:

```
sudo ./jitsu www.openmirage.org,192.168.0.22,mirage-www -c xen:/// 
```

The command above connects to a local Xen-server (from dom0) through libvirt and starts the DNS server. Requests for www.openmirage.org will be redirected to the Xen-VM called "mirage-www" with IP 192.168.0.22. If "mirage-www" is not running, jitsu will start it automatically before responding to the DNS request.

See ./jitsu --help for more options.


