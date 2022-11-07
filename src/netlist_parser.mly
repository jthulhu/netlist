%{
 open Netlist_ast

 let bool_of_string s = match s with
  | "t" | "1" -> true
  | "f" | "0" -> false
  | _ -> raise Parsing.Parse_error

 let bool_array_of_string s =
   let a = Array.make (String.length s) false in
   for i = 0 to String.length s - 1 do
     a.(i) <- bool_of_string (String.sub s i 1)
   done;
   a

 let value_of_const s =
   let n = String.length s in
   if n = 0 then
     raise Parsing.Parse_error
   else if n = 1 then
     VBit (bool_of_string s)
   else
     VBitArray (bool_array_of_string s)
%}

%token <string> CONST
%token <string> NAME
%token AND MUX NAND OR RAM ROM XOR REG NOT
%token CONCAT SELECT SLICE
%token COLON EQUAL COMMA VAR IN INPUT OUTPUT
%token EOF

%start program             /* the entry point */
%type <Netlist_ast.program> program

%%
program:
  INPUT inp=separated_list(COMMA, NAME)
    OUTPUT out=separated_list(COMMA, NAME)
    VAR vars=separated_list(COMMA, var) IN eqs=list(equ) EOF
    { { eqs; vars = Env.of_list vars; inputs = inp; outputs = out; } }

equ:
  x=NAME EQUAL e=exp { (x, e) }

exp:
  | a=arg { Arg a }
  | NOT x=arg { Not x }
  | REG x=NAME { Reg x }
  | AND x=arg y=arg { Binop(And, x, y) }
  | OR x=arg y=arg { Binop(Or, x, y) }
  | NAND x=arg y=arg { Binop(Nand, x, y) }
  | XOR x=arg y=arg { Binop(Xor, x, y) }
  | MUX x=arg y=arg z=arg { Mux(x, y, z) }
  | ROM addr=int word=int ra=arg
    { Rom(addr, word, ra) }
  | RAM addr=int word=int ra=arg we=arg wa=arg data=arg
    { Ram(addr, word, ra, we, wa, data) }
  | CONCAT x=arg y=arg
     { Concat(x, y) }
  | SELECT idx=int x=arg
     { Select (idx, x) }
  | SLICE min=int max=int x=arg
     { Slice (min, max, x) }

arg:
  | n=CONST { Const (value_of_const n) }
  | id=NAME { Var id }

var: x=NAME ty=ty_exp { (x, ty) }
ty_exp:
  | /*empty*/ { TBit }
  | COLON n=int { TBitArray n }

int:
  | c=CONST { int_of_string c }
