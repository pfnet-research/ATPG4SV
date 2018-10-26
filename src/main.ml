
let read_from_flie filename =
  let ic = open_in filename in
  let rec read_loop acc =
    match input_line ic with
    | s -> Printf.sprintf "%s\n%s" s (read_loop acc)
    | exception End_of_file -> acc 
  in read_loop ""

let parse_with_error lexbuf = 
  let open Lexer in
  try Parser.start token lexbuf with
  | Syntax_error msg ->
    Printf.fprintf stderr "Syntax_error : %s \n" msg; exit(-1)
  | Parser.Error -> Printf.fprintf stderr "syntax error \n"; exit (-1)

let parsed_tree file_name =
  file_name
  |> read_from_flie 
  |> Lexing.from_string
  |> parse_with_error

let dump_next_inputs oc_inputs oc inputs =
  let vars = List.map (fun (_, var) ->
      let l = String.length var - 2 in
      l, String.(sub var 2 l)) inputs in
  let max_len = List.fold_left (fun acc (l, _) -> 
      if l > acc then l else acc) 0 vars in
  let res = List.fold_left
      (fun acc (l, x) -> Printf.sprintf "%s%s" acc 
        @@ Printf.sprintf "%s%s\n" (String.make (max_len - l) '0') x) ""
      vars in
  let () = match oc_inputs with
    | None -> ()
    | Some oc -> Printf.fprintf oc "%s%s\n" res (String.make max_len '-') in
  let () = Printf.fprintf oc "%s" res; flush_all () in
  close_out oc

let gen_initial oc_inputs oc input_ports =
  let input_ports = List.map (fun (dt, id) -> id, Ir.gen_random dt) input_ports in        
  dump_next_inputs oc_inputs oc input_ports;
  Printf.printf "Generated initial inputs\n"

(* dump new inputs and update CFGs *)
let gen_inputs oc_inputs test dtrace dz3 vcd dict assigns input_ports cfgs oc =
  let cfgs, cl = Constraint.gen_constraints test dtrace vcd (dict, assigns, cfgs) in         
  let () = dump_next_inputs oc_inputs oc @@ Solve.solve dz3 dict cl input_ports in
  cfgs

let gen_inputs oc_inputs test dtrace dz3 vcd dict assigns input_ports cfgs oc =
  try (gen_inputs oc_inputs test dtrace dz3 vcd dict assigns input_ports cfgs oc) with
  | e -> failwith @@ Printexc.to_string e

let run_simulation test script =
  let cmd = if test then "make sim_add_pure" else Printf.sprintf "bash %s" script in 
  let () = ignore @@ Sys.command cmd in ()

let () =
  (* CLI *)
  let in_file_name = ref "" in
  let dparse = ref false in
  let dir = ref false in  
  let dcfg = ref false in
  let dtrace = ref false in
  let dz3 = ref false in
  let dcov = ref false in
  let dinputs = ref "" in
  let df = ref "" in
  let dv = ref "" in
  let ds = ref "" in
  let max_iter = ref 10 in
  let test = ref false in  
  let set_str t s = t := s in
  let set_true t () = t := true in
  let () = Arg.parse 
      [("-i", Arg.String (set_str in_file_name), "input file");
       ("-m", Arg.Int (fun i -> max_iter := i), "set the max number of iterations");
       ("-f", Arg.String (set_str df), "path to a file to store input vectors");
       ("-v", Arg.String (set_str dv), "path to a vcd file");
       ("-s", Arg.String (set_str ds), "path to a shell script to run simulator");
       ("-c", Arg.Unit (set_true dcov), "dump coverage information");
       ("-dparse", Arg.Unit (set_true dparse), "dump parsed tree");
       ("-dir", Arg.Unit (set_true dir), "dump IR");
       ("-dcfg", Arg.Unit (set_true dcfg), "generate dot file of CFG");
       ("-dtrace", Arg.Unit (set_true dtrace), "dump trace");
       ("-dz3", Arg.Unit (set_true dz3), "dump z3 log");
       ("-dinputs", Arg.String (set_str dinputs), "dump all input vectors");
       ("-test", Arg.Unit (set_true test), "run without concrete execution")
      ]  print_endline "Usage: _build/default/src/main.exe [options] -i systemverilog_file -f input_vectors_file -v vcd -s script\nOptions:" in

  (* check arguments *)
  let () =
    if !in_file_name = "" then (Printf.printf "input file is needed\n"; exit 0)
    else if !df = "" then (Printf.printf "file to store input vectors is needed\n"; exit 0)
    else if !dv = "" then (Printf.printf "vcd file is needed\n"; exit 0)
    else if !ds = "" then (Printf.printf "script to run simulator is needed\n"; exit 0)
    else () in

  let in_file_name_without_ext =  Filename.chop_extension !in_file_name in

  (* tmp file for next inputs *)
  let inputs_file = !df in
  let oc_inputs = if !dinputs = "" then None else Some (open_out !dinputs) in

  (* common data *)
  let dict, assigns, cfgs, input_ports =
    let parsedtree = parsed_tree !in_file_name in
    let ir = Ir.of_module_declaration parsedtree in
    let dict, assigns, cfgs, input_ports = Cfg.of_module_declaration ir in
    let () = if !dparse 
      then Printf.printf "%s\n" (Parsetree.show_module_declaration parsedtree) in
    let () = if !dir then Printf.printf "%s\n" (Ir.show_module_declaration ir) in
    let () = if !dcfg then 
        List.iteri (fun i x -> Vis.visualize (Printf.sprintf "%s%d.dot" in_file_name_without_ext i) x) cfgs in
    dict, assigns, cfgs, List.rev input_ports in

  let gen_inputs = gen_inputs oc_inputs !test !dtrace !dz3 !dv dict assigns input_ports in

  (* dump initial inputs *)
  let () = gen_initial oc_inputs (open_out inputs_file) input_ports in

  let num_nodes = List.fold_left (fun acc x -> acc + Cfg.counts_on_nodes (fun _ -> true) x) 0 cfgs in

  let rec loop n m cfgs =
    if n > m then ()
    else 
      let () = Printf.printf "================================================\n" in
      let () = Printf.printf "Starts loop %d/%d\n" n m in
      let () = Printf.printf "Starts concrete execution\n" in      
      let () = run_simulation !test !ds in
      let () = Printf.printf "Starts generating next inputs\n" in
      let cfgs = gen_inputs cfgs (open_out inputs_file) in 
      let () = if !dcfg then 
          List.iteri (fun i x -> Vis.visualize (Printf.sprintf "%s%d_%d.dot" in_file_name_without_ext i n) x) cfgs in
      let num_covered_nodes = List.fold_left (fun acc x -> acc + Cfg.counts_on_nodes (fun bb -> bb.Cfg.reached) x) 0 cfgs in   
      let () = 
        if !dcov then Printf.printf "coverage = %d / %d\n" num_covered_nodes num_nodes in
      if num_covered_nodes >= num_nodes then Printf.printf "Complete!\n" else loop (n + 1) m cfgs in

  (* start loop *)
  loop 1 !max_iter cfgs
