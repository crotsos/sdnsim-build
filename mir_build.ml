open Printf
open Xml
open Lwt
open Lwt_unix

let nodes = Hashtbl.create 64 
let links = ref [] 
let duration = ref 60

type output_type = 
  | NS3
  | XEN

let string_to_output_type = function
  | "ns3-direct" -> NS3
  | "xen" -> XEN
  | a -> failwith (sprintf "unknown output %s" a)

let output_target = ref NS3
let module_name = ref ""

let get_attrib_fail el name = 
  try 
    List.assoc name el
  with Not_found -> 
    failwith (sprintf "required attribute %s undefined" name)

let get_attrib_default el name default_value = 
  try 
    List.assoc name el 
  with Not_found -> default_value

let find_all el name = 
  let is_el name = function
    | Xml.Element(a, _, _) when (a=name) -> true
    | _ -> false
  in 
    List.filter (is_el name) el 

let parse_xm_file file = 
  let _ = printf "opening file %s...\n%!" file in 
  let _ = 
    match (Xml.parse_file file) with 
      | Xml.Element("topology", a, c) -> 
          let _ = module_name := (get_attrib_fail a "module") in  
          let _ = duration := int_of_string (get_attrib_default a "duration" "60") in  
          let _ =  output_target := 
                   string_to_output_type 
                     (get_attrib_default a "backend" "ns3-direct") in
          let _ = 
            List.iter (
              fun x -> 
                match x with
                | (Xml.Element(_, attrs, children)) ->  begin
                    try
                      let node = get_attrib_fail attrs "name" in
                      let main_method = get_attrib_fail attrs "main" in
                      let params = 
                        List.fold_right 
                          (fun n r -> 
                             match n with
                               | Xml.Element("param", a, [(Xml.PCData v)]) -> 
                                   begin try 
                                     r @ [(Some(get_attrib_fail a "name"), v)]
                                   with ex -> r @  [(None, v)]
                                   end
                               | _ ->
                                   printf "cannot parse param %sn%!" (Xml.to_string n); 
                                   r 
                          ) children [] in 
                      let _ = Hashtbl.add nodes node (main_method, params) in 
                        printf "read %s -> %s\n%!" node main_method 
                    with ex -> 
                      printf "failed to parse node %s\n%!" (Xml.to_string x)
                  end
                | _ -> ()
            ) (find_all c "node") in 
          let _ = 
            List.iter 
              (fun el -> 
                 match el with
                   | Xml.Element("link", el, _) ->
                       begin 
                         try
                           let node_src = get_attrib_fail el "src" in
                           let node_dst = get_attrib_fail el "dst" in
                           let delay = int_of_string (get_attrib_default el "delay" "10") in 
                           let rate = int_of_string (get_attrib_default el "rate" "100") in 
                           let pcap = bool_of_string (get_attrib_default el "pcap" "false") in
                             if ((Hashtbl.mem nodes node_src) &&
                                 (Hashtbl.mem nodes node_dst)) then 
                               let _ = links := !links @  
                                       [(node_src, node_dst, delay, rate, pcap)] in 
                                 printf "read %s -> %s (%d, %d, %B)\n%!" 
                                   node_src node_dst delay rate pcap
                                 else
                                   printf "Node %s or %s not define, ignoming links...\n" 
                                     node_src node_dst
                         with ex -> ()
                       end
                   | _ -> ()
              ) (find_all c "link") in
            ()
      | _ -> ()
  in
   ()

let run_code () = 
  match !output_target with
    | NS3 -> Ns3.run_code !module_name nodes !links 
    | XEN -> Xen.run_code !module_name !duration nodes !links

lwt _ =
  try
    let _ = parse_xm_file Sys.argv.(1) in
    lwt _ = run_code () in  
      return ()
  with ex ->
      return (eprintf "error: %s\n%s\n%!" (Printexc.to_string ex) (Printexc.get_backtrace ()))
