open Netlist_ast

exception WrongInputSize of int * int
exception WrongInputBit of char
exception WrongCharacterInRom of string * int * int * char
exception RomTooSmall of string * int * int

let file = ref ""
let number_steps = ref (-1)
let batch_mode = ref false
  
let simulator program number_steps =
  let size = List.length program.eqs in
  let rams = Hashtbl.create size in
  let roms = Hashtbl.create size in
  let registers = Hashtbl.create size in
  let env = Hashtbl.create size in
  let init_env key ty =
    let add key value tbl =
      Hashtbl.add tbl key value in
    match ty with
    | TBit ->
      [env; registers]
      |> List.iter (add key (VBit false))
    | TBitArray taille ->
      [env; registers]
      |> List.iter (add key (VBitArray (Array.make taille false))) in
  Env.iter init_env program.vars;
  let init_rams (key, exp) =
    match exp with
    | Ram (addr_size, word_size, _, _, _, _) ->
      Array.init (1 lsl addr_size) (fun _ -> Array.make word_size false)
      |> Hashtbl.add rams key
    | Rom (addr_size, word_size, _) ->
      let rom_file = Filename.chop_suffix !file "net" ^ key ^ ".rom" in
      let full_size = (1 lsl addr_size) * word_size in
      let fd = open_in rom_file in
      let content =
        let lit_ligne ligne s =
          String.iteri (fun colonne c -> match c with
              | '0' | '1' | '\n' -> ()
              | c ->
                raise (WrongCharacterInRom (rom_file, ligne, colonne, c))) s;
          s in
        In_channel.input_all fd
        |> String.split_on_char '\n'
        |> List.to_seq
        |> Seq.mapi lit_ligne
        |> List.of_seq
        |> String.concat "" in
      close_in fd;
      if String.length content > full_size then
        raise (RomTooSmall (rom_file, String.length content, full_size));
      let rom =
        Array.init (1 lsl addr_size) (fun _ -> Array.make word_size false) in
      for i = 0 to (1 lsl addr_size) - 1 do
        for j = 0 to word_size - 1 do
          let idx = i*word_size + j in
          if idx < String.length content then
            rom.(i).(j) <-
              begin
                match content.[idx] with
                | '0' -> false
                | '1' -> true
                | _ -> failwith "N'est pas sensé arriver"
              end
        done
      done;
      Hashtbl.add roms key rom
    | _ -> () in
  begin
    try
      List.iter init_rams program.eqs
    with
    | RomTooSmall (file, file_size, wanted_size) ->
      Printf.printf "%s is too big (%d) to fit in ROM of size %d.\n"
        file file_size wanted_size;
      exit 1
    | WrongCharacterInRom (file, ligne, colonne, char) ->
      Printf.printf "%s, at line %d, character %d ('%c') is not a valid bit.\n"
        file ligne colonne char;
      exit 1
  end;
  let set key value =
    Hashtbl.replace env key value in
  let get key =
    Hashtbl.find env key in
  (* let get_old key = *)
  (*   Hashtbl.find registers key in *)
  let get_bool = function
    | VBit b -> b
    | VBitArray _ -> failwith "Array à la place d'un booléen." in
  let get_array = function
    | VBitArray arr -> arr
    | VBit b -> [| b |] in
  let eval_arg ?(get_f=get) = function
    | Var ident ->
      (* Printf.printf "%s\n" ident; *)
      get_f ident
    | Const value ->
      (* Printf.printf "lol\n"; *)
      value in
  let get_number ?(get_f=get) arg =
     let arr = arg |> eval_arg ~get_f |> get_array in
    let res = ref 0 in
    for i = 0 to Array.length arr - 1 do
      if arr.(i) then
        res := !res + (1 lsl i)
    done;
    !res in
  let eval_expr writes key = function
    | Arg arg -> eval_arg arg
    | Reg ident -> Hashtbl.find registers ident
    | Not arg ->
      let r = eval_arg arg |> get_bool |> not in
      VBit r
    | Binop (op, left, right) ->
      let l = eval_arg left |> get_bool in
      let r = eval_arg right |> get_bool in
      VBit begin match op with
        | Or -> l || r
        | Xor -> l <> r
        | And -> l && r
        | Nand -> not (l && r)
      end
    | Mux (choice, a, b) ->
      let c = get_bool (eval_arg choice) in
      (if c then b else a) |> eval_arg
    | Ram (_, _, read_addr, write_enable, write_addr, data) ->
      let result = Array.copy (Hashtbl.find rams key).(get_number read_addr) in
      if write_enable |> eval_arg ~get_f:get |> get_bool then
          writes := (key, write_addr, data) :: !writes;
        (* (Hashtbl.find rams key).(get_number write_addr) <- *)
        (*   data |> eval_arg ~get_f:get_old |> get_array; *)
      VBitArray result
    | Rom (_, _, read_addr) ->
      let result = (Hashtbl.find roms key).(get_number read_addr) in
      VBitArray result
    | Concat (left, right) ->
      let l = match eval_arg left with
        | VBit b -> [| b |]
        | VBitArray v -> v in
      let r = match eval_arg right with
        | VBit b -> [| b |]
        | VBitArray v -> v in
      let n = Array.length l in
      let m = Array.length r in
      let res = Array.init (n+m) (fun i -> if i >= n then r.(i-n) else l.(i)) in
      VBitArray res
    | Slice (s, e, arg) ->
      let inp = eval_arg arg |> get_array in
      let size = e-s+1 in
      VBitArray (Array.init size (fun i -> inp.(s+i)))
    | Select (i, arg) ->
      VBit ((eval_arg arg |> get_array).(i)) in
  let exec_instr writes (ident, expr) =
    set ident (eval_expr writes ident expr) in
  let print_outout ident =
    Printf.printf "=> %s = " ident;
    match get ident with
    | VBit false -> print_endline "0"
    | VBit true -> print_endline "1"
    | VBitArray arr ->
      print_string "<";
      for i = 0 to Array.length arr - 1 do
        if arr.(i) then
          print_string "1"
        else
          print_string "0"
      done;
      print_endline ">" in
  let rec read_input key =
    try
      let size = match Env.find key program.vars with
        | TBit -> 1
        | TBitArray size -> size in
      if not !batch_mode then
        Printf.printf "%s(%d) = " key size;
      let r = match Env.find key program.vars with
        | TBit ->
          begin
            match read_line () with
            | "0" -> VBit false
            | "1" -> VBit true
            | _ -> failwith "Wrong input."
          end
        | TBitArray size ->
          let inp = read_line () in
          if String.length inp <> size then
            raise (WrongInputSize (String.length inp, size));
          inp
          |> String.to_seq
          |> Seq.map (function
              | '0' -> false
              | '1' -> true
              | c -> raise (WrongInputBit c))
          |> Array.of_seq
          |> fun x -> VBitArray x in
      set key r
    with
    | WrongInputBit c ->
      Printf.printf "'%c' is not a valid bit.\n" c;
      read_input key
    | WrongInputSize (expected, actual) ->
      Printf.printf "Expected input of size %d, got %d\n" expected actual;
      read_input key in
  for _ = 1 to number_steps do
    let writes = ref [] in
    List.iter read_input program.inputs;
    List.iter (exec_instr writes) program.eqs;
    List.iter print_outout program.outputs;
    List.iter begin
      fun (key, wa, data) ->
        (Hashtbl.find rams key).(get_number wa) <- data |> eval_arg ~get_f:get |> get_array
    end !writes;
    Hashtbl.iter (fun key value -> Hashtbl.replace registers key value) env
  done

let compile filename =
  file := filename;
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end
  with
  | Netlist.Parse_error s ->
    Format.eprintf "An error accurred: %s@." s;
    exit 2

let () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate";
     "--batch", Arg.Set batch_mode, "Whether to run in batch mode"]
    compile
    ""
