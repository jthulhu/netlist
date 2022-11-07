open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp eq =
  let read_arg arg acc = match arg with
    | Var ident -> ident::acc
    | _ -> acc in
  let aux acc = function
    | Arg arg | Not arg | Rom (_, _, arg)
    | Slice (_, _, arg) | Select (_, arg)
    | Ram (_, _, arg, _, _, _) ->
      read_arg arg acc
    | Reg _ident -> (* ident::acc *) acc
    | Binop (_, arg, arg') | Concat (arg, arg') ->
      read_arg arg acc |> read_arg arg'
    | Mux (arg, arg', arg'') ->
      acc
      |> read_arg arg
      |> read_arg arg'
      |> read_arg arg'' in
  aux [] (snd eq)

let schedule p =
  let g = mk_graph () in
  Env.iter (fun ident _ -> add_node g ident) p.vars;
  List.iter begin
    fun (ident, arg) ->
      read_exp (ident, arg)
      |> List.iter (fun dep -> add_edge g dep ident)
  end p.eqs;
  let rec add acc = function
    | [] -> { p with eqs = acc }
    | ident::tail ->
      try
        let eq = List.find (fun (ident', _) -> ident = ident') p.eqs in
        add (eq::acc) tail
      with Not_found -> add acc tail in
  topological g |> add [] 
