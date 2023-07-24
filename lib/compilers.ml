open Vm_ast
open Symbol_table
(* compilation helpers *)
type 'a compiler = symbolTable -> ('a * symbolTable)
type jcompiler = (vmCommand list) compiler
let return vm : jcompiler = fun table -> (vm, table)
let return_a v : 'a compiler = fun table -> (v, table)
let ( >>| ) comp f = fun table -> let (a, table') = (comp table) in (f a, table')
let ( >>= ) comp fcomp = fun table ->
    let (a, table') = (comp table) in (fcomp a table')
let ( *> ) comp1 comp2 = comp1 >>= (fun _ -> comp2)
let map f comp = comp >>| f
let bind comp fcomp = comp >>= fcomp
let both comp1 comp2 = fun table ->
        let (vm1, table')  = comp1 table in
        let (vm2, table'') = comp2 table' in
                  ((vm1, vm2), table'')

let sequence (comp1:jcompiler) (comp2:jcompiler) : jcompiler = fun table ->
        let (vm1, table') = comp1 table in
        let (vm2, table'') = comp2 table' in
                (vm1 @ vm2, table'')
let ( <+> ) = sequence

let sum_over_list (comp : 'a -> jcompiler) (l : 'a list) : jcompiler = fun table -> List.fold_left
        (fun (vm, table) tree ->
            let (vm', table') = (comp tree table)
            in (vm @ vm', table'))
        ([], table) l
let iterate_over_list (comp : 'a -> unit compiler) (l : 'a list) : unit compiler = fun table -> ((),
        List.fold_left (fun table tree ->
            let ((), table') = (comp tree table)
            in table')
        table l)

(* do only if eq is true *)
let ite (cond : bool) (then_comp : 'a compiler) (else_comp : 'a compiler) : 'a compiler =
        fun table -> if cond then then_comp table else else_comp table
let ite_full (cond_comp : bool compiler) (then_comp : 'a compiler) (else_comp : 'a compiler) : 'a compiler =
        cond_comp >>= (fun c -> ite c then_comp else_comp)

(* table accessors and mutators *)
let mutate_table ftable = fun table -> ((), ftable table)
let access_table ftable = fun table -> (ftable table, table)
let lookup_opt (sym:string) : (symbolData option) compiler = fun table -> (lookup_sym sym table, table)
(* let lookup sym = lookup_opt sym >>| try Option.get with _ -> raise (Invalid_argument ("Bad lookup for symbol " ^ sym)) *)
let lookup sym = fun table -> try (Option.get @@ lookup_sym sym table, table) with _ -> raise (Invalid_argument ("Bad lookup for symbol " ^ sym))
let next_label_suffix = fun table ->
    (string_of_int table.labelCount, {table with labelCount = table.labelCount+1})
let next_label prefix = next_label_suffix >>| (fun s -> prefix ^ s)

module Let_syntax = struct
  let return = return
  let ( >>| ) = ( >>| )
  let ( >>= ) = ( >>= )

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both
    (* let map2 = map2 *)
    (* let map3 = map3 *)
    (* let map4 = map4 *)
  end
end

let ( let+ ) = ( >>| )
let ( let* ) = ( >>= )
let ( and+ ) = both

exception CompilationError of string

