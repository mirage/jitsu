(*
 * Copyright (c) 2014 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Cmdliner

let info =
  let doc =
    "Just-In-Time Summoning of Unikernels. Jitsu is a forwarding DNS server \
     that automatically starts unikernel VMs when their domain is requested. \
     The DNS response is sent to the client after the unikernel has started, \
     enabling the client to use unmodified software to communicate with \
     unikernels that are started on demand. If no DNS requests are received \
     for the unikernel within a given timeout period, the VM is automatically \
     stopped." in
  let man = [
    `S "EXAMPLES";
    `P "jitsu -f 8.8.8.8 -m destroy mirage.org,virbr0,10.0.0.1,/unikernels/mirage-www.xen,32768";
    `P "Start unikernel '/unikernels/mirage-www.xen' with 32MiB of memory and a VIF on virbr0 on requests for mirage.org and \
        return IP 10.0.0.1 when VM is running. Forward unknown requests to \
        8.8.8.8 (Google). Expired VMs are destroyed.";
    `S "AUTHORS";
    `P "Magnus Skjegstad <magnus@skjegstad.com>" ;
    `S "BUGS";
    `P "Submit bug reports to http://github.com/magnuss/jitsu";] in
  Term.info "jitsu" ~version:"0.1-alpha" ~doc ~man

let bindaddr =
  let doc = "Bind local DNS server to interface with this IP" in
  Arg.(value & opt string "127.0.0.1" & info ["b"; "bind"] ~docv:"ADDR" ~doc)

let bindport =
  let doc = "UDP port to listen for DNS queries" in
  Arg.(value & opt int 53 & info ["l"; "listen"] ~docv:"PORT" ~doc)

let forwarder =
  let doc =
    "IP address of DNS server queries should be forwarded to if no local match \
     is found. Defaults to system default (/etc/resolv.conf) if not specified."
  in
  Arg.(value & opt string "" & info ["f" ; "forwarder"] ~docv:"ADDR" ~doc)

let forwardport =
  let doc = "UDP port to forward DNS queries to" in
  Arg.(value & opt int 53 & info ["p"; "port"] ~docv:"PORT" ~doc)

let response_delay =
  let doc =
    "Time to wait in seconds before responding to a DNS query after the local \
     VM has started. This delay gives the VM time to open up the necessary TCP \
     ports etc. Setting this value too low can result in long delays on the \
     first TCP request to the VM." in
  Arg.(value & opt float 0.1 & info ["d" ; "delay" ] ~docv:"SECONDS" ~doc)

let bridge =
  let doc = "Bridge to attach VM NICs to" in
  Arg.(value & opt string "xenbr0" & info [ "bridge" ] ~docv:"BRIDGE" ~doc)

let map_domain =
  let doc =
    "Maps DOMAIN to a VM, IP and memory in KiB. VM must match a VM available through libvirt \
     (see virsh list --all)." in
  Arg.(non_empty & pos_all (t4 ~sep:',' string string string int64) [] & info []
         ~docv:"DOMAIN,IP,BRIDGE,VM,KIB" ~doc)

let ttl =
  let doc =
    "DNS TTL in seconds. The TTL determines how long the clients may cache our \
     DNS response. VMs are terminated after no DNS requests have been received \
     for TTL*2 seconds." in
  Arg.(value & opt int 60 & info ["t" ; "ttl" ] ~docv:"SECONDS" ~doc)

let vm_stop_mode =
  let doc =
    "How to stop running VMs after timeout. Valid options are $(b,suspend), \
     $(b,destroy) and $(b,shutdown). Suspended VMs are generally faster to \
     resume, but require resources to store state. Note that Mirage \
     suspend/resume is currently not supported on ARM." in
  Arg.(value & opt (enum [("destroy" , Jitsu.VmStopDestroy);
                          ("suspend" , Jitsu.VmStopSuspend);
                          ("shutdown", Jitsu.VmStopShutdown)])
         Jitsu.VmStopSuspend & info ["m" ; "mode" ] ~docv:"MODE" ~doc)

let log m =
  Printf.fprintf stdout "%s%!" m

let or_abort f =
  try f () with
  | Failure m -> (Printf.fprintf stderr "Fatal error: %s" m); exit 1

let or_warn msg f =
  try f () with
  | Failure m -> (log (Printf.sprintf "Warning: %s\nReceived exception: %s" msg m)); ()

let spinner = [| '-'; '\\'; '|'; '/' |]

let jitsu bindaddr bindport forwarder forwardport response_delay
    bridge map_domain ttl vm_stop_mode =
  let maintenance_thread t timeout =
    let rec loop i =
      let i = if i >= Array.length spinner then 0 else i in
      Lwt_unix.sleep timeout >>= fun () ->
      Printf.printf "\b%c%!" spinner.(i);
      Jitsu.stop_expired_vms t
      >>= fun () ->
      loop (i + 1) in
    loop 0 in
  Lwt_main.run (
    ((match forwarder with
        | "" -> Dns_resolver_unix.create () (* use resolv.conf *)
        | _  -> let forwardip = Ipaddr.of_string_exn forwarder in (* use ip from forwarder *)
          let servers = [(forwardip,forwardport)] in
          let config = `Static ( servers , [""] ) in
          Dns_resolver_unix.create ~config:config ()
      )
     >>= fun forward_resolver ->
     let t = Jitsu.create log forward_resolver ttl in
     Lwt.choose [(
         (* main thread, DNS server *)
         let per_vm (dns,ip,name,memory_kb) =
           log (Printf.sprintf "Adding domain '%s' for VM '%s' with ip %s on bridge %s and %Ld KiB of RAM\n" dns name ip bridge memory_kb);
           Jitsu.add_vm t ~domain:dns ~name ~bridge ~memory_kb (Ipaddr.V4.of_string_exn ip)
             vm_stop_mode ~delay:response_delay ~ttl
         in
         Lwt_list.iter_p per_vm map_domain
         >>= fun () ->
         log (Printf.sprintf "Starting server on %s:%d...\n" bindaddr bindport);
         let processor = ((Dns_server.processor_of_process (Jitsu.process t))
                          :> (module Dns_server.PROCESSOR)) in
         Dns_server_unix.serve_with_processor ~address:bindaddr ~port:bindport
           ~processor);
        (* maintenance thread, delay in seconds *)
        (maintenance_thread t 5.0)]);
  )

let jitsu_t =
  Term.(pure jitsu $ bindaddr $ bindport $ forwarder $ forwardport
        $ response_delay $ bridge $ map_domain $ ttl $ vm_stop_mode )

let () =
  match Term.eval (jitsu_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
