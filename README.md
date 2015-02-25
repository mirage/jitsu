# Just-In-Time Summoning of Unikernels #

Jitsu is a forwarding DNS server that automatically starts virtual machines (VMs) on demand. When a DNS query is received, jitsu first checks for a local VM that is mapped to the requested domain. If a VM is found, the VM is started and its IP is returned to the client. Otherwise, the request is forwarded to the next DNS server. If no DNS requests are received for the VM within a given timeout period it is automatically stopped. 

Jitsu is mainly intended for use with Xen
[unikernels](http://www.openmirage.org) that can be started quickly and be able to respond to the client request within the time it takes to send the DNS response. 

## Getting started ##

Jitsu can be installed using opam:

```
opam pin add jitsu 'https://github.com/MagnusS/jitsu.git#dev'
opam install jitsu
```

If the installation succeeds you should now be able to start jitsu:

```
sudo ./jitsu --nics=xenbr0 name=openmirage,domain=www.openmirage.org,ip=192.168.0.22,kernel=/unikernels/mirage-www.xen,mem=32768
```

The command above connects to a local Xen-server (from dom0) through libxl and starts the DNS server.
Requests for www.openmirage.org will be redirected to the Xen unikernel with filename
`/unikernels/mirage-www.xen` configured with 32MiB of RAM and with IP 192.168.0.22.
If `/unikernels/mirage-www.xen` is not running, jitsu will start it automatically before
responding to the DNS request.

See [below](#options) or run ./jitsu --help for more options.

## Options ##

```
-b ADDR, --bind=ADDR (absent=127.0.0.1)
   Bind local DNS server to interface with this IP

--bridge=BRIDGE (absent=xenbr0)
   Bridge to attach VM NICs to

-d SECONDS, --delay=SECONDS (absent=0.1)
   Time to wait in seconds before responding to a DNS query after the
   local VM has started. This delay gives the VM time to open up the
   necessary TCP ports etc. Setting this value too low can result in
   long delays on the first TCP request to the VM.

-f ADDR, --forwarder=ADDR
   IP address of DNS server queries should be forwarded to if no local
   match is found. Defaults to system default (/etc/resolv.conf) if
   not specified.

--help[=FMT] (default=pager)
   Show this help in format FMT (pager, plain or groff).

-l PORT, --listen=PORT (absent=53)
   UDP port to listen for DNS queries

-m MODE, --mode=MODE (absent=suspend)
   How to stop running VMs after timeout. Valid options are suspend,
   destroy and shutdown. Suspended VMs are generally faster to resume,
   but require resources to store state. Note that Mirage
   suspend/resume is currently not supported on ARM.

-p PORT, --port=PORT (absent=53)
   UDP port to forward DNS queries to

-t SECONDS, --ttl=SECONDS (absent=60)
   DNS TTL in seconds. The TTL determines how long the clients may
   cache our DNS response. VMs are terminated after no DNS requests
   have been received for TTL*2 seconds.

--version
   Show version information.
```
