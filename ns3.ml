open Printf
open Lwt
open Lwt_unix

let lwt_open_out file = 
  openfile file [O_WRONLY; O_SYNC] 0o777 

let lwt_output_string fd str = 
  lwt _ = write fd str 0 (String.length str) in 
    return ()

let lwt_close_out fd = 
  close fd

let generate_simulation module_name nodes links =
  lwt out = lwt_open_out (sprintf "topo_%s.ml" module_name) in
  lwt _ = lwt_output_string out "let run () =\n" in  
  lwt _ = Lwt_list.iter_p (
    fun (host, main, params) ->
      let str_param = 
        List.fold_right (
        fun (n, v) r -> 
          match n with
            | None -> sprintf "%s %s" r v
            | Some(n) -> sprintf "%s ~%s:%s" r n v
        ) params "" in 
      let str_node = 
        sprintf "\tlet _ = OS.Topology.add_node \"%s\" (%s.%s %s) in\n" 
          host module_name main str_param in
        lwt_output_string out str_node
  ) (Hashtbl.fold (fun host (main, params) r-> r @  [(host, main, params)]) nodes []) in
  lwt _ = 
    Lwt_list.iter_s
      ( fun (node_src, node_dst, delay, rate, pcap) -> 
          let str_node = 
            sprintf "\tlet _ = OS.Topology.add_link ~prop_delay:%d ~rate:%d ~pcap:%s \"%s\" \"%s\" in"
              delay rate (string_of_bool pcap) node_src node_dst in
            lwt_output_string out str_node
      ) links in
  lwt _ = lwt_output_string out "\t\t()\n" in 
  lwt _ = lwt_close_out out in 
  lwt out = lwt_open_out (sprintf "topo_%s.mir" module_name) in
  lwt _ = lwt_output_string out 
            (sprintf "Topo_%s.run \n"
            module_name) in  
    lwt_close_out out 

let build_simulation module_name = 
  lwt _ = Lwt_unix.system "ocamlbuild -clean" in 
  lwt _ = Lwt_unix.system (sprintf "mir-build ns3-direct/topo_%s.bin" module_name) in 
    return ()

let run_simulation module_name = 
  lwt _ = Lwt_unix.system (sprintf "./_build/ns3-direct/topo_%s.bin" module_name) in 
    return ()

let run_code module_name nodes links = 
  lwt _ = generate_simulation module_name nodes links in
  lwt _ = build_simulation module_name in 
  lwt _ = run_simulation module_name in 
    return ()
 
