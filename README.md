# Just-In-Time Summoning of Unikernels #

Jitsu is a forwarding DNS server that automatically boots [unikernels](http://www.openmirage.org) on demand. When a DNS query is received, Jitsu first checks for a local unikernel that is mapped to the requested domain. If a unikernel is found, the unikernel is started and its IP is returned to the client. Otherwise, the request is forwarded to the next DNS server. If no DNS requests are received for the unikernel within a given timeout period it is automatically stopped. 

Although Jitsu can be used to control other types of virtual machines, it is mainly intended for use with unikernels that can be booted quickly and be able to respond to the client request within the time it takes to send the DNS response. 

Jitsu supports several backends to manage the unikernel VMs. Currently [libvirt](https://libvirt.org), [XAPI](http://wiki.xenproject.org/wiki/XAPI) and [libxenlight](http://wiki.xen.org/wiki/Choice_of_Toolstacks#Libxenlight_.28libxl.29) are supported. Metadata and internal state is stored in [Irmin](https://github.com/mirage/irmin) and the DNS server is implemented on top of [ocaml-dns](https://github.com/mirage/ocaml-dns).

## Installing Jitsu ##

Jitsu needs the development libraries for Libvirt, Xapi and Xenlight to compile. This can be handled by `opam` in most cases. To install the system dependencies, run

```
opam depext xenctrl libvirt xen-api-client
```

To compile the latest development version of Jitsu, pin Jitsu to the current master and run install:

```
opam pin add jitsu 'https://github.com/mirage/jitsu.git'
opam install jitsu
```

If the installation succeeds you should now be able to start Jitsu. If you ran into problems and are using OS X, see below for additional troubleshooting tips.

### OS X troubleshooting ###
To install the OCaml libvirt bindings on OS X we have to set CPPFLAGS first (due to this [bug](https://github.com/ocaml/opam-repository/issues/2621)). This step can be skipped on other platforms.

```
CPPFLAGS="-Wno-error=tautological-compare -Wno-error=unused-function"  opam install libvirt
```

You should now be able to install Jitsu as usual.

## Getting started ##

Jitsu is initially launched with a list of unikernels, their configurations and a set of parameters that define 

 - how to connect to the virtualization backend
 - how the DNS server should be configured
 - how the unikernels should be managed

A minimal Jitsu configuration could look like this:

```
sudo ./jitsu dns=www.openmirage.org,ip=192.168.0.22,name=mirage-www -c xen:///
```

The command above connects to a local Xen-server (from dom0) through libvirt (the default) and starts the DNS server. Requests for www.openmirage.org will be redirected to the Xen-VM called "mirage-www" with IP 192.168.0.22. If "mirage-www" is not running, Jitsu will start it automatically before responding to the DNS request.

Each unikernel is configured using a set of key/value pairs separated by commas. The parameters that are supported depends on which virtualization backend (libvirt, xapi or libxl) is used to control the unikernels. See [below](#options) or run ./jitsu --help for a complete set of options.

### Example: Suspend/resume Linux VMs in Virtualbox ###
Jitsu can be used to control VMs in Virtualbox with libvirt. Note that how well this will work depends on how quickly the VM is able to respond to requests after resuming from suspend (see also the `-d` parameter for how to delay the DNS response).

First, install libvirt and use virsh to display a list of available VMs. Example output:

```
$ virsh list --all
Welcome to virsh, the virtualization interactive terminal.

Type:  'help' for help with commands
       'quit' to quit

virsh # list --all
 Id    Name                           State
----------------------------------------------------
 2     Ubuntu                         running
```

If virsh is unable to connect to Virtualbox, you may have to adjust the connection URI (`-c [uri]`). The default connection URI for Virtualbox is vbox:///session - see [this page](https://libvirt.org/remote.html) for more details. In OS X you may have to set the socket manually, which can be done with 'vbox:///session?socket=path-to-socket'. Remember to use the same connection URI for Jitsu below.

You should now be able to start Jitsu. Use '-m suspend' to set it to suspend the VM on inactivity. Example output:

```
$ sudo ./jitsu www.example.com,127.0.0.1,Ubuntu -m suspend -c vbox:///session
Connecting to vbox:///session...
Adding domain 'www.example.com' for VM 'Ubuntu' with ip 127.0.0.1
Adding SOA 'example.com' with ttl=60
Adding A PTR for 'www.example.com' with ttl=60 and ip=127.0.0.1
Starting server on 127.0.0.1:53...
```

To test that Jitsu works, try to resolve the domain with host:

```
$ host www.example.com 127.0.0.1
Using domain server:
Name: 127.0.0.1
Address: 127.0.0.1#53
Aliases:

www.example.com has address 127.0.0.1
```

The domain should now be running.

```
$ virsh dominfo Ubuntu
Id:             2
Name:           Ubuntu
UUID:           6e696eb7-09f4-484c-981b-8d34efa0304d
OS Type:        hvm
State:          running
CPU(s):         3
Max memory:     2147483648 KiB
Used memory:    3166208 KiB
Persistent:     yes
Managed save:   unknown
```

After 2 minutes without DNS requests, Jitsu will suspend the domain automatically. This timeout can be adjusted with the --ttl parameter. Jitsu terminates the VM after 2 x DNS TTL seconds has expired without new requests.

## Options ##
```
    -b ADDR, --bind=ADDR (absent=127.0.0.1)
        Bind local DNS server to interface with this IP

    -c CONNECT, --connect=CONNECT (absent=xen:///)
        Libvirt and Xapi connection string (e.g. xen+ssh://x.x.x.x/system
        or vbox:///session)

    -d SECONDS, --delay=SECONDS (absent=0.1)
        Time to wait in seconds before responding to a DNS query after the
        local VM has started. This delay gives the VM time to open up the
        necessary TCP ports etc. Setting this value too low can result in
        long delays on the first TCP request to the VM.

    -f ADDR, --forwarder=ADDR
        IP address of DNS server queries should be forwarded to if no local
        match is found. Defaults to system default (/etc/resolv.conf) if
        not specified. Set to 0.0.0.0 to disable forwarding.

    --help[=FMT] (default=pager)
        Show this help in format FMT (pager, plain or groff).

    -l PORT, --listen=PORT (absent=53)
        UDP port to listen for DNS queries

    -m MODE, --mode=MODE (absent=destroy)
        How to stop running VMs after timeout. Valid options are suspend,
        destroy and shutdown. Suspended VMs are generally faster to resume,
        but require resources to store state. Note that MirageOS
        suspend/resume is currently not supported on ARM.

    -p PORT, --port=PORT (absent=53)
        UDP port to forward DNS queries to

    --synjitsu=UUID
        UUID of a running Synjitsu compatible unikernel. When specified,
        Jitsu will attempt to connect to a Synjitsu unikernel over Vchan on
        port 'synjitsu' and send notifications with updates on MAC- and
        IP-addresses of booted unikernels. This allows Synjitsu to send
        gratuitous ARP on behalf of booting unikernels and to cache
        incoming SYN packets until they are ready to receive them. This
        feature is experimental and requires a patched MirageOS TCP/IP
        stack.

    -t SECONDS, --ttl=SECONDS (absent=60)
        DNS TTL in seconds. The TTL determines how long the clients may
        cache our DNS response. VMs are terminated after no DNS requests
        have been received for TTL*2 seconds.

    --version
        Show version information.

    -x BACKEND, --backend=BACKEND (absent=libvirt)
        Which backend to use. Currently libvirt, xapi and libxl are
        supported. Xapi and libxl are less tested and should be considered
        experimental.

LIBVIRT CONFIGURATION
    name
        Name of VM defined in libvirt (ignored if uuid is set)

    uuid
        UUID of VM defined in libvirt (required, but optional if name is
        set)

    dns DNS name (required)

    ip  IP to return in DNS reply (required)

XAPI CONFIGURATION
    name
        Name of VM defined in Xapi (ignored if uuid is set)

    uuid
        UUID of VM defined in Xapi (optional if name is set)

    dns DNS name (required)

    ip  IP to return in DNS reply (required)

LIBXL CONFIGURATION
    name
        Name of created VM (required)

    dns DNS name (required)

    ip  IP to return in DNS reply (required)

    kernel
        VM kernel file name (required)

    memory
        VM memory in bytes (required)

    cmdline
        Extra parameters passed to kernel (optional)

    nics
        Network devices (br0, eth0 etc) (optional, only one supported)

    scripts
        VIF script(s) (optional, only one supported)
```

## License ##
```
Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com> and contributors.

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```
