open Printf
open Xml
open Lwt
open Lwt_unix

let nodes = Hashtbl.create 64 
let links = ref [] 

type output_type = 
  | NS3
  | XEN

let string_to_output_type = function
  | "ns3-direct" -> NS3
  | "xen-direct" -> XEN
  | a -> failwith (sprintf "unknown output %s" a)

let output_target = ref NS3
let module_name = ref ""

let get_attrib_fail el name = 
  try 
    Xml.attrib el name
  with Xml.No_attribute _ -> 
    failwith (sprintf "required attribute %s undefined" name)

let get_attrib_default el name default_value = 
  try 
    Xml.attrib el name 
  with Xml.No_attribute _ -> default_value

let parse_xm_file file = 
  let _ = printf "opening file %s...\n%!" file in 
  let xml = Xml.parse_file file in
  let _ = module_name := (get_attrib_fail xml "module") in  
 let _ =  output_target := 
          string_to_output_type 
            (get_attrib_default xml "backend" "ns3-direct") in 
  let _ = List.iter (
      fun el -> 
        try
          if ((Xml.tag el) = "node" ) then
            let node = get_attrib_fail el "name" in
            let main_method = get_attrib_fail el "main" in
            let params = 
              Xml.fold 
                (fun r el -> 
                   let v = (Xml.pcdata (List.hd (Xml.children el))) in
                     if ((Xml.tag el) = "param") then
                       try 
                         r @ [(Some(Xml.attrib el "name"), v)]
                       with Xml.No_attribute _ -> r @  [(None, v)]
                         else
                           r
                ) [] el in 
            let _ = Hashtbl.add nodes node (main_method, params) in 
              printf "read %s -> %s\n%!" node main_method 
        with Xml.No_attribute _ -> ()
    ) (Xml.children xml) in 
  let _ = 
    List.iter 
      (fun el ->
        try
          if ((Xml.tag el) = "link" ) then
            let node_src = get_attrib_fail el "src" in
            let node_dst = get_attrib_fail el "dst" in
            let delay = int_of_string (get_attrib_default el "delay" "10") in 
            let rate = int_of_string (get_attrib_default el "rate" "100") in 
            let pcap = bool_of_string (get_attrib_default el "pcap" "false") in
              if ((Hashtbl.mem nodes node_src) &&
                  (Hashtbl.mem nodes node_dst)) then 
                let _ = links := !links @  
                        [(node_src, node_dst, delay, rate, pcap)] in 
                  printf "read %s -> %s (%d, %d, %B)\n%!" node_src node_dst delay rate pcap
              else
                  printf "Node %s or %s not define, ignoming links...\n" node_src node_dst
        with Xml.No_attribute _ -> ()
      ) (Xml.children xml) in
    ()

let lwt_open_out file = 
  openfile file [O_WRONLY; O_SYNC] 0o777 

let lwt_output_string fd str = 
  lwt _ = write fd str 0 (String.length str) in 
    return ()

let lwt_close_out fd = 
  close fd

let generate_simulation () =
  lwt out = lwt_open_out (sprintf "topo_%s.ml" !module_name) in
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
          host !module_name main str_param in
        lwt_output_string out str_node
  ) (Hashtbl.fold (fun host (main, params) r-> r @  [(host, main, params)]) nodes []) in
  lwt _ = 
    Lwt_list.iter_s
      ( fun (node_src, node_dst, delay, rate, pcap) -> 
          let str_node = 
            sprintf "\tlet _ = OS.Topology.add_link ~prop_delay:%d ~rate:%d ~pcap:%s \"%s\" \"%s\" in"
              delay rate (string_of_bool pcap) node_src node_dst in
            lwt_output_string out str_node
      ) !links in
  lwt _ = lwt_output_string out "\t\t()\n" in 
  lwt _ = lwt_close_out out in 
  lwt out = lwt_open_out (sprintf "topo_%s.mir" !module_name) in
  lwt _ = lwt_output_string out 
            (sprintf "Topo_%s.run \n"
            !module_name) in  
    lwt_close_out out 

let build_simulation () = 
  lwt _ = Lwt_unix.system "ocamlbuild -clean" in 
  lwt _ = Lwt_unix.system (sprintf "mir-build ns3-direct/topo_%s.bin" !module_name) in 
    return ()

let run_simulation () = 
  lwt _ = Lwt_unix.system (sprintf "./_build/ns3-direct/topo_%s.bin" !module_name) in 
    return ()

let run_code () = 
  match !output_target with
    | NS3 -> 
        lwt _ = generate_simulation () in
        lwt _ = build_simulation () in 
        lwt _ = run_simulation () in 
          return ()
    | _ -> return ()

lwt _ =
  try
    let _ = parse_xm_file Sys.argv.(1) in
    lwt _ = run_code () in 
      return ()
  with ex ->
      return (eprintf "error: %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ()))
