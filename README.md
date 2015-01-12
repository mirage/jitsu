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

See [below](#Options) or run ./jitsu --help for more options.

### Testing with VMs in Virtualbox ###
Jitsu can be used to control VMs in Virtualbox. First, install libvirt and use virsh to display a list of available VMs. Example output:

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

If virsh is unable to connect to Virtualbox, you may have to adjust the connection URI (`-c [uri]`). The default connection URI for Virtualbox is vbox:///session - see [this page](https://libvirt.org/remote.html) for more details. I had to set the socket manually in OS X, which can be done with 'vbox:///session?socket=path-to-socket'. Remember to use the same connection URI for Jitsu below.

You should now be able to start Jitsu. Use '-m suspend' to set it to suspend the VM on inactivity. Example output:

```bash
$ sudo ./jitsu www.example.com,127.0.0.1,Ubuntu -m suspend -c vbox:///session
Connecting to vbox:///session...
Adding domain 'www.example.com' for VM 'Ubuntu' with ip 127.0.0.1
Adding SOA 'example.com' with ttl=60
Adding A PTR for 'www.example.com' with ttl=60 and ip=127.0.0.1
Starting server on 127.0.0.1:53...
```

To test that Jitsu works, try to resolve the domain with host:

```bash
$ host www.example.com 127.0.0.1
Using domain server:
Name: 127.0.0.1
Address: 127.0.0.1#53
Aliases:

www.example.com has address 127.0.0.1
```

The domain should now be running.

```bash
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

After 2 minutes without DNS requests, Jitsu will suspend the domain automatically. This timeout can be set with the --ttl parameter.

## Options ##

```
-b ADDR, --bind=ADDR (absent=127.0.0.1)
           Bind local DNS server to interface with this IP

-c CONNECT, --connect=CONNECT (absent=xen:///)
   libvirt connection string (e.g. xen+ssh://x.x.x.x/system or
   vbox:///session)

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

