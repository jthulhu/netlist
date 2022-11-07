type ident = string

(* Environment using ident as key *)
module Env = struct
  include Map.Make(struct
    type t = ident
    let compare = compare
  end)

  let of_list l =
    List.fold_left (fun env (x, ty) -> add x ty env) empty l
end

type ty =
  | TBit
  | TBitArray of int
      
type value =
  | VBit of bool
  | VBitArray of bool array

type binop = Or | Xor | And | Nand

(* argument of operators (variable or constant) *)
type arg =
    | Var of ident (* x *)
    | Const of value (* constant *)

(* Expressions (see MiniJazz documentation for more info on the operators) *)
type exp =
    | Arg of arg (* a: argument *)
    | Reg of ident (* REG x : register *)
    | Not of arg (* NOT a *)
    | Binop of binop * arg * arg (* OP a1 a2 : boolean operator *)
    | Mux of arg * arg * arg (* MUX a1 a2 : multiplexer *)
    | Rom of int (*addr size*) * int (*word size*) * arg (*read_addr*)
        (* ROM addr_size word_size read_addr *)
    | Ram of int (*addr size*) * int (*word size*)
        * arg (*read_addr*) * arg (*write_enable*)
        * arg (*write_addr*) * arg (*data*)
        (* RAM addr_size word_size read_addr write_enable write_addr data *)
    | Concat of arg * arg (* CONCAT a1 a2 : concatenation of arrays *)
    | Slice of int * int * arg
      (* SLICE i1 i2 a : extract the slice of a between indices i1 and i2 *)
    | Select of int * arg
      (* SELECT i a : ith element of a *)

(* equations: x = exp *)
type equation = ident * exp

type program = {
  eqs : equation list; (* equations *)
  inputs : ident list; (* inputs *)
  outputs : ident list; (* outputs *)
  vars : ty Env.t;
} (* maps variables to their types*)
