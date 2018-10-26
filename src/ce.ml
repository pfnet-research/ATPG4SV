
(* 
 Concrete Execution 
 Generates execution path from VCD.
*)

let read_ce ic =
  let rec aux l =
    match input_line ic with
    | s -> aux (s::l)
    | exception _ -> l in
  aux [] |> List.rev

type value = Cint of int | Cfloat of float | Cundef | Cbool of bool
[@@deriving show]
type time = int
[@@deriving show]
type log = (time * value) list
[@@deriving show]
type var = {name: string; hname: string; type_: string; length: int; arrdim: (int * int) option; log:log; last:value}
[@@deriving show]

(* utility functions *)

let tb_prefix = ["sv_filename"; "sv_classname"]

let hname_of_name x = 
  String.concat "." @@ tb_prefix @ [x]

let rec name_of_hname = function
  | [] -> failwith "name_of_hname"
  | [x] -> x
  | x::xs -> Printf.sprintf "%s.%s" x (name_of_hname xs)

let int_of_bin s =
  if String.contains s 'x' then Cundef else Cint (int_of_string @@ "0" ^ s)

let bit_extract x h l =
  let y = x lsr l in
  let z = 1 lsl (h - l + 1) - 1 in
  y land z

let bit_length x =  
  let rec aux n m =
    if n - 1 >= x then m 
    else aux (n lsl 1) (m + 1) in
  aux 2 1

let bit_concat x y =
  let l = bit_length y in
  (x lsl l) lor y

let exp_extract value : Ir.dimension -> value =
  let v = (match value with
      | Cint i -> i
      | _ -> failwith "unexpected value for extract") in
  function
  | Dimension_range (EInt(1, a), EInt(1, b)) -> Cint (bit_extract v a b)
  | Dimension_index (EInt(1, a)) -> Cint ((v lsr a) land 1)
  | _ -> failwith "unexpected value for extract 2"


(* vcd is assumed to be in the following structure:
    $var <type> <length> <sym> <name> [<arrdim>] $end

   two dictionaries are generated from vcd:
   - bd:  <sym> -> {<name>; <hierarchical_name>; <type>; <length>; <arrdim>; log}
          - log -> (time, value) list
   - ndb; <hierarchical_name> -> sym *)

(* read a line from vcd file and add information to dictionaries(ndb, db) *)
let process_line ndb db h t s =
  let l = Str.(split (regexp " +") s) in
  match l with
  | ["$var"; type_; length; sym; name; "$end"] ->
    let hname = name_of_hname @@ List.rev (name::h) in
    let r = {name=name;
             hname;
             type_=type_;
             length=int_of_string length; 
             arrdim=None;
             log=[];
             last=Cundef} in
    Hashtbl.add db sym r; 
    Hashtbl.add ndb hname sym;
    (ndb, db, h, t)
  | ["$var"; type_; length; sym; name; range; "$end"] ->
    let rl, rr = (match Str.(split (regexp ":") range) with
        | [rl; rr] -> rl, rr
        | _ -> failwith "unexpected format") in
    let rl = String.(sub rl 1 (length rl - 1)) |> int_of_string in
    let rr = String.(sub rr 0 (length rr - 1)) |> int_of_string in
    let hname = name_of_hname @@ List.rev (name::h) in
    let r = {name=name;
             hname;
             type_=type_;
             length=int_of_string length; 
             arrdim=Some(rl, rr);
             log=[];
             last=Cundef} in 
    Hashtbl.add db sym r; 
    Hashtbl.add ndb hname sym;
    (ndb, db, h, t)
  | ["$scope"; _; name; "$end"] -> (ndb, db, name :: h, t)
  | ["$upscope"; "$end"] -> (ndb, db,  (try List.tl h with _ -> []), t)
  | [tt] when String.contains tt '#' ->
    let time = try String.(sub tt 1 (length tt - 1)) |> int_of_string with
      | Failure _ -> t in
    (ndb, db, h, time)

  (* each line of the value change section starts from 0, 1, x, b, or r *)
  | [v] when t >= 0 && String.length v > 1 && String.get v 0 = '0' ->
    let l = (t, Cint 0) in
    let s = String.sub v 1 (String.length s - 1) in
    let r = try Hashtbl.find db s with Not_found -> failwith (Printf.sprintf "process_line: ce: not found %s" s) in
    Hashtbl.replace db s { r with log = (l :: r.log) }; (ndb, db, h, t)
  | [v] when t >= 0 && String.length v > 1 && String.get v 0 = '1' ->
    let l = (t, Cint 1) in
    let s = String.sub v 1 (String.length s - 1) in
    let r = try Hashtbl.find db s with Not_found -> failwith (Printf.sprintf "process_line: ce: not found: %s" s) in
    Hashtbl.replace db s { r with log = (l :: r.log) }; (ndb, db, h, t)
  | [v; s] when t >= 0 && String.length v > 1 && String.get v 0 = 'b' ->
    let l = (t, int_of_bin v) in
    let r = try Hashtbl.find db s with Not_found -> failwith (Printf.sprintf "process_line: ce: not found %s" s) in
    Hashtbl.replace db s { r with log = (l :: r.log) }; (ndb, db, h, t)
  | [v; s] when t >= 0 && String.length v > 1 && String.get v 0 = 'r' ->
    let l = (t, Cfloat (float_of_string (String.sub v 1 (String.length v - 1)))) in
    let r = try Hashtbl.find db s with Not_found -> failwith (Printf.sprintf "process_line: ce: not found %s" s) in
    Hashtbl.replace db s { r with log = (l :: r.log) }; (ndb, db, h, t)
  | [v; s] when t >= 0 && String.length v > 1 && String.get v 0 = 'x' ->
    let l = (t, Cundef) in
    let r = try Hashtbl.find db s with Not_found -> failwith (Printf.sprintf "process_line: ce: not found %s" s) in
    Hashtbl.replace db s { r with log = (l :: r.log) }; (ndb, db, h, t)
  | _ -> (ndb, db, h, t)

let process_lines ss =
  let db = Hashtbl.create 100 in
  let ndb = Hashtbl.create 100 in
  let rec aux ndb db h t = function
    | [] -> (ndb, db, h, t)
    | s :: ss -> 
      let (ndb, db, h, t) = process_line ndb db h t s in
      aux ndb db h t ss in
  aux ndb db [] (-1) ss

let calc_last : var -> var = fun v ->
  {v with last = try snd @@ List.hd v.log with _ -> Cundef}

(* generate dictionaries from vcd file *)
let gen_dbs filename =
  let ic = open_in filename in 
  let ss = read_ce ic in
  let (ndb, db, _, _) = process_lines ss in
  let () = Hashtbl.iter (fun x y -> Hashtbl.replace db x (calc_last y)) db in
  let n2vdb = Hashtbl.create 100 in (* <hierarchical_name> -> <value> *)
  let () = Hashtbl.iter (fun hn s -> 
      try 
        let v = Hashtbl.find db s in
        Hashtbl.add n2vdb hn v.last 
      with _ -> ()) ndb in
  let () = close_in ic in  
  n2vdb

(* functions to calcurate concrete execution path *)

let rec exp_execution db : Ir.expression -> value = fun x ->
  match x with
  | EInt (_, i) -> Cint i
  | Efloat f -> Cfloat f
  | Estring s ->
    let s = hname_of_name s in
    (try Hashtbl.find db s with
     | _ -> failwith 
       @@ Printf.sprintf "exp_execution: ce: could not find %s" s)
  | Eident (hn, d) ->
    let id = Ir.id_of_hierarchical hn in
    let id = hname_of_name id in
    let res = (try Hashtbl.find db id with
        | _ -> failwith 
          @@ Printf.sprintf "exp_execution: ce: could not find %s" id) in
    (match d with
     | None -> res
     | Some d -> exp_extract res d)
  | Econcat es -> 
    let ress = List.map (exp_execution db) es in
    let i = List.fold_left (fun acc x -> 
        (match x with
         | Cint x -> bit_concat acc x
         | _ -> failwith "exp_execution: ce: unexpected value for Econcat")
      ) 0 ress in 
    Cint i
  | Eunary (op, e) ->
    let v = exp_execution db e in
    (match op with
     | "!" | "~" ->  (match v with
         | Cint i -> Cint (lnot i)
         | Cbool b -> Cbool (not b)
         | _ -> failwith "exp_execution: ce: unexpected value for Eunary"
       )
     | _ ->  failwith "exp_execution: ce: unexpected value for Eunary")
  | Ebinary (e1, op, e2) -> (* TODO: too concice *)
    let e1, e2 = exp_execution db e1, exp_execution db e2 in
    (match op with
     | "+" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 + i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "-" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 - i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "<<" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 lsl i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | ">>" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 lsr i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "||" | "|" -> (match e1, e2 with 
         | Cint i1, Cint i2 -> Cint (i1 lor i2)
         | Cbool b1, Cbool b2 -> Cbool (b1 || b2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "&&" | "&" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 land i2)
         | Cbool b1, Cbool b2 -> Cbool (b1 && b2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "==" -> Cbool (e1 = e2)
     | ">" -> (match e1, e2 with
         | Cint i1, Cint i2 -> Cbool (i1 > i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "<" ->  (match e1, e2 with
         | Cint i1, Cint i2 -> Cbool (i1 < i2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | "^" ->  (match e1, e2 with
         | Cint i1, Cint i2 -> Cint (i1 lor i2)
         | Cbool b1, Cbool b2 -> Cbool (b1 <> b2)
         | _ -> failwith "exp_execution: ce: unexpected value for Ebinary")
     | _ -> failwith "exp_execution: ce: unexpected operator for Ebinary"
    )
  | Econd (p, e1, e2) ->
    let p, e1, e2 = exp_execution db p, exp_execution db e1, exp_execution db e2 in
    if p = Cint 1 || p = Cbool true then e1 else e2

let rec fml_execution db : Cfg.fml -> bool = fun x ->
  (* let () = Printf.printf "f> %s\n" (Cfg.string_of_fml x) in *)
  match x with
  | Fexp e -> let res = exp_execution db e in
    res = Cbool true || res = Cint 1
  | Feq (e1, e2) -> (exp_execution db  e1) = (exp_execution db e2)
  | Fnot f -> not @@ fml_execution db f
  | Fdisj (f1, f2) -> (fml_execution db f1) || (fml_execution db f2)
  | Fconj (f1, f2) -> (fml_execution db f1) && (fml_execution db f2)
  | Ftrue -> true

let concrete_execution trace db : (Cfg.fml * Cfg.bb) list -> int * (Cfg.fml * Cfg.bb) =
  fun bbs ->
    let rec aux n = function
      | [] -> Printf.printf "concrete_execution: ce: cannot decide next branch\n";
        raise @@ Failure "ce"
      | (f,bb)::fs -> 
        let () = 
          if trace then Printf.printf "formula is %s\n" @@ Cfg.show_fml f in
        if fml_execution db f then 
          let () = if trace then Printf.printf "branch %d/%d is taken\n" (n+1) (List.length bbs) in
          n, (f, bb)
        else 
          aux (n+1) fs in
    try aux 0 bbs with
    | Failure _ -> (
        let n = List.length bbs in
        let () = Random.init (Sys.time() *. 10000. |> int_of_float) in
        let next = Random.int n in
        next, List.nth bbs next)

let gen_ce_path test trace vcd : Cfg.cfg -> int list = 
  let filename = if test then "log.vcd" else vcd in
  let db = gen_dbs filename in
  let rec aux trace db : Cfg.cfg -> int list = fun (bb, ext) ->
    match bb.bb_children with
    | [] -> []
    | _ ->
      if Cfg.next_is_exit bb.bb_children ext then
        []
      else
        let n, (_, b) = concrete_execution trace db bb.bb_children in
        let ns = aux trace db (b, ext) in 
        n :: ns
  in aux trace db
