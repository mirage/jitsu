(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com>
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
     that automatically boots unikernels when their domain is requested. \
     The DNS response is sent to the client after the unikernel has started, \
     enabling the client to use unmodified software to communicate with \
     unikernels that are started on demand. If no DNS requests are received \
     for the unikernel within a given timeout period, the unikernel is automatically \
     stopped." in
  let list_options l =
    List.map (fun x ->
        let (k,v) = x in
        `I ((Printf.sprintf "$(b,%s)" k), v)) l in
  let common_options =
    [ ("response_delay", "Override default DNS query response delay for this unikernel. See also -d.") ;
      ("wait_for_key", "Wait for this key to appear in Xenstore before responding to the DNS query. Sleeps for [response_delay] after the key appears. The key should be relative to /local/domain/[domid]/.") ;
      ("use_synjitsu", "Enable Synjitsu for this domain if not 0 or absent (requires Synjitsu support enabled)") ] in
  let man =
    [ `S "COMMON CONFIGURATION" ] @ (list_options common_options) @
    [ `S "LIBVIRT CONFIGURATION" ] @ (list_options Libvirt_backend.get_config_option_list) @
    [ `S "XAPI CONFIGURATION" ] @ (list_options Xapi_backend.get_config_option_list) @
    [ `S "LIBXL CONFIGURATION" ] @ (list_options Libxl_backend.get_config_option_list) @
    [ `S "EXAMPLES";
      `P "$(b,jitsu -c xen:/// -f 8.8.8.8 dns=mirage.io,ip=10.0.0.1,vm=mirage-www)" ;
      `P "Connect to Xen via libvirt. Start unikernel $(b,mirage-www) on requests for $(b,mirage.io) and \
          return IP $(b,10.0.0.1) in DNS. Forward unknown requests to \
          $(b,8.8.8.8).";
      `P "$(b,jitsu -c vbox:///session -m suspend dns=home.local,ip=192.168.0.1,name=ubuntu -t 60)";
      `P "Connect to Virtualbox. Start VM $(b,ubuntu) on requests for $(b,home.local) \
          and return IP $(b,192.168.0.1). Forward unknown requests to system default. \
          Expired VMs are $(b,suspended) after $(b,120) seconds (2 x DNS ttl).";
      `S "AUTHORS";
      `P "Magnus Skjegstad <magnus@skjegstad.com>" ;
      `S "BUGS";
      `P "Submit bug reports to http://github.com/mirage/jitsu";] in
  Term.info "jitsu" ~version:"0.2-alpha" ~doc ~man

let bindaddr =
  let doc = "Bind local DNS server to interface with this IP" in
  Arg.(value & opt string "127.0.0.1" & info ["b"; "bind"] ~docv:"ADDR" ~doc)

let bindport =
  let doc = "UDP port to listen for DNS queries" in
  Arg.(value & opt int 53 & info ["l"; "listen"] ~docv:"PORT" ~doc)

let connstr =
  let doc =
    "Libvirt and Xapi connection string (e.g. xen+ssh://x.x.x.x/system or vbox:///session)"
  in
  Arg.(value & opt string "xen:///" & info ["c"; "connect"] ~docv:"CONNECT" ~doc)

let forwarder =
  let doc =
    "IP address of DNS server queries should be forwarded to if no local match \
     is found. Defaults to system default (/etc/resolv.conf) if not specified. \
     Set to 0.0.0.0 to disable forwarding."
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

let map_domain =
  let doc =
    "Unikernel configuration. Maps DNS and IP to a unikernel VM. Configuration \
     options are passed as keys and values in the form \"key1=value1,key2=value2...\". \
     A configuration string must be specified for each \
     unikernel Jitsu should control. \
     Required keys are $(b,name), $(b,dns) and $(b,ip). \
     Depending on the selected virtualization backend, additional keys may be supported. \
     See full list of available keys below." in
  Arg.(non_empty & pos_all (array ~sep:',' (t2 ~sep:'=' string string)) [] & info []
         ~docv:"CONFIG" ~doc)

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
     resume, but require resources to store state. Note that MirageOS \
     suspend/resume is currently not supported on ARM." in
  Arg.(value & opt (enum [("destroy" , Vm_stop_mode.Destroy);
                          ("suspend" , Vm_stop_mode.Suspend);
                          ("shutdown", Vm_stop_mode.Shutdown)])
         Vm_stop_mode.Destroy & info ["m" ; "mode" ] ~docv:"MODE" ~doc)

let synjitsu_domain_uuid =
  let doc =
    "UUID of a running Synjitsu compatible unikernel. When specified, \
     Jitsu will attempt to connect to a Synjitsu unikernel over Vchan on port 'synjitsu' \
     and send notifications with updates on MAC- and IP-addresses of booted \
     unikernels. This allows Synjitsu to send gratuitous ARP on behalf of \
     booting unikernels and to cache incoming SYN packets until they are \
     ready to receive them. This feature is $(b,experimental) and requires a patched \
     MirageOS TCP/IP stack."  in
  Arg.(value & opt (some string) None & info ["synjitsu"] ~docv:"UUID" ~doc)

let persistdb =
  let doc =
    "Store the Irmin database in the specified path. The default is to store the database in memory only. \
     Note that modifying this database while Jitsu is running is currently unsupported and may crash Jitsu
     or have other unexpected results." in
  Arg.(value & opt (some string) None & info [ "persistdb" ] ~docv:"path" ~doc)

let log m =
  Printf.fprintf stdout "%s\n%!" m

let or_abort f =
  try f () with
  | Failure m -> (Printf.fprintf stderr "Fatal error: %s" m); exit 1

let or_warn msg f =
  try f () with
  | Failure m -> (log (Printf.sprintf "Warning: %s\nReceived exception: %s" msg m)); ()
  | e -> (log (Printf.sprintf "Warning: Unhandled exception: %s" (Printexc.to_string e))); ()

let or_warn_lwt msg f =
  try_lwt f () with
  | Failure m -> (log (Printf.sprintf "Warning: %s\nReceived exception: %s" msg m)); Lwt.return_unit
  | e -> (log (Printf.sprintf "Warning: Unhandled exception: %s" (Printexc.to_string e))); Lwt.return_unit

let backend =
  let doc =
    "Which backend to use. Currently libvirt, xapi and libxl are supported. Xapi and libxl \
     are less tested and should be considered experimental." in
  Arg.(value & opt (enum [("libvirt" , `Libvirt);
                          ("libxl", `Libxl);
                          ("xapi", `Xapi)])
         `Libvirt & info ["x" ; "backend" ] ~docv:"BACKEND" ~doc)


let jitsu backend connstr bindaddr bindport forwarder forwardport response_delay
    map_domain ttl vm_stop_mode synjitsu_domain_uuid persistdb =
  let (module Vm_backend : Backends.VM_BACKEND) =
    match backend with
    | `Libvirt -> (module Libvirt_backend)
    | `Xapi -> (module Xapi_backend)
    | `Libxl -> (module Libxl_backend)
  in
  let (module Storage_backend : Backends.STORAGE_BACKEND) = 
    match persistdb with
    | None -> (module Irmin_backend.Make(Irmin_unix.Irmin_git.Memory))
    | Some _ -> (module Irmin_backend.Make(Irmin_unix.Irmin_git.FS))
  in
  let module Jitsu = Jitsu.Make(Vm_backend)(Storage_backend) in
  let rec maintenance_thread t timeout =
    Lwt_unix.sleep timeout >>= fun () ->
    Printf.printf "%s%!" ".";
    or_warn_lwt "Unable to stop expired VMs" (fun () -> Jitsu.stop_expired_vms t) >>= fun () ->
    maintenance_thread t timeout;
  in
  Lwt.async_exception_hook := (fun exn -> log (Printf.sprintf "Exception in async thread: %s" (Printexc.to_string exn)));
  Lwt_main.run (
    ((match forwarder with
        | "" -> Dns_resolver_unix.create () >>= fun r -> (* use resolv.conf *)
          Lwt.return (Some r)
        | "0.0.0.0" -> Lwt.return None
        | _  -> let forwardip = Ipaddr.of_string_exn forwarder in (* use ip from forwarder *)
          let servers = [(forwardip,forwardport)] in
          let config = `Static ( servers , [""] ) in
          Dns_resolver_unix.create ~config:config () >>= fun r ->
          Lwt.return (Some r)
      )
     >>= fun forward_resolver ->
     let connstr = Uri.of_string connstr in
     let synjitsu =
       match synjitsu_domain_uuid with
       | Some s -> Uuidm.of_string s
       | None -> None
     in
     Vm_backend.connect ~connstr () >>= fun r ->
     match r with
     | `Error e -> raise (Failure (Printf.sprintf "Unable to connect to backend: %s" (Jitsu.string_of_error e)))
     | `Ok backend_t ->
       or_abort (fun () -> Jitsu.create backend_t log forward_resolver ~synjitsu ~persistdb ()) >>= fun t ->
       Lwt.pick [(
           (* main thread, DNS server *)
           let add_with_config config_array = (
             let vm_config = (Hashtbl.create (Array.length config_array)) in
             (Array.iter (fun (k,v) -> Hashtbl.add vm_config k v) config_array); (* Use .add to support multiple values per parameter name *)
             let dns_names = Options.get_dns_name_list vm_config "dns" in
             let vm_name = Options.get_str vm_config "name" in
             let vm_ip = Options.get_ipaddr vm_config "ip" in
             let response_delay =  (* override default response_delay if key set in config *)
               match (Options.get_float vm_config "response_delay") with
               | `Error _ -> response_delay
               | `Ok d -> d
             in
             let use_synjitsu =
               match (Options.get_bool vm_config "use_synjitsu"), synjitsu with
               | `Error _, _
               | `Ok _, None -> false (* default to false if use_synjitsu is not set or synjitsu is not enabled *)
               | `Ok v, Some _ -> v
             in
             let wait_for_key =
               match Options.get_str vm_config "wait_for_key" with
               | `Error _ -> None
               | `Ok v -> Some v
             in
             match dns_names, vm_name, vm_ip with
             | `Error e, _, _
             | _, `Error e, _
             | _, _, `Error e -> raise (Failure (Options.string_of_error e))
             | `Ok dns_names, `Ok vm_name, `Ok vm_ip -> begin
                 match (Ipaddr.to_v4 vm_ip) with
                 | None -> raise (Failure (Printf.sprintf "Only IPv4 is supported. %s is not a valid IPv4 address." (Ipaddr.to_string vm_ip)))
                 | Some vm_ip -> begin
                     List.iter (fun dns_name ->
                         log (Printf.sprintf "Adding domain '%s' for VM '%s' with ip %s" (Dns.Name.to_string dns_name) vm_name (Ipaddr.V4.to_string vm_ip)))
                       dns_names;
                     or_abort (fun () -> Jitsu.add_vm t ~dns_names:dns_names ~vm_ip ~vm_stop_mode ~response_delay ~wait_for_key ~use_synjitsu ~dns_ttl:ttl ~vm_config)
                   end
               end
           ) in
           Lwt_list.iter_s add_with_config map_domain >>= fun () ->
           Jitsu.output_stats t () >>= fun () ->
           log (Printf.sprintf "Starting DNS server on %s:%d..." bindaddr bindport);
           try_lwt
             let processor = ((Dns_server.processor_of_process (Jitsu.process t))
                              :> (module Dns_server.PROCESSOR)) in
             Dns_server_unix.serve_with_processor ~address:bindaddr ~port:bindport ~processor
           with
           | e -> log (Printf.sprintf "DNS thread exited unexpectedly with exception: %s" (Printexc.to_string e)); Lwt.return_unit
             >>= fun () ->
             log "DNS server no longer running. Exiting...";
             Lwt.return_unit);

          (* maintenance thread, delay in seconds *)
          (try_lwt
             maintenance_thread t 5.0
           with
           | e -> log (Printf.sprintf "Maintenance thread exited unexpectedly with exception: %s" (Printexc.to_string e)); Lwt.return_unit
             >>= fun () ->
             log "Maintenance thread no longer running. Exiting...";
             Lwt.return_unit)]);
  )

let jitsu_t =
  Term.(pure jitsu $ backend $ connstr $ bindaddr $ bindport $ forwarder $ forwardport
        $ response_delay $ map_domain $ ttl $ vm_stop_mode $ synjitsu_domain_uuid $ persistdb )

let () =
  match Term.eval (jitsu_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
