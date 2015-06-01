open Lwt
open Irmin_unix

type t = {
    connection : string -> ([ `BC ], Irmin.Contents.String.Path.t, Irmin.Contents.String.t) Irmin.t;
}

type id = string

let create ?persist:(persist=true) ?root:(root="irmin/test") () =
    let store = match persist with
    | true -> Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String) 
    | false -> Irmin.basic (module Irmin_git.Memory) (module Irmin.Contents.String)
    in
    let config = Irmin_git.config ~root ~bare:true () in
    Irmin.create store config task >>= fun connection ->
    Lwt.return { connection } 


let add_vm t ~vm_name ~dns_name ~ip:ip_addr ~stop_mode ~delay:response_delay ~ttl =
    let it = t.connection in
    Irmin.update (it "Registering new domain") ["jitsu" ; "vm" ; vm_name ; "dns" ; "name" ] dns_name >>= fun () ->
    Irmin.update (it "...")                    ["jitsu" ; "vm" ; vm_name ; "dns" ; "ip" ] ip_addr >>= fun () ->
    Irmin.update (it "....")                   ["jitsu" ; "vm" ; vm_name ; "stop_mode" ] (Vm_stop_mode.to_string stop_mode) >>= fun () ->
    Lwt.return_unit
