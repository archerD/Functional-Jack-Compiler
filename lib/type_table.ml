(* type checking *)
(* type table: map of class to class types,
 * class type: list of type varibles, map of variables to type and binding, and functions to types and function type *)
open Jack_ast

module TypeTable = Map.Make(String)

type classType = 
    { type_variables : string list option
    ; variables : (jackType * jackBinding) TypeTable.t
    ; functions : ((string * jackType) list * jackType option * (string * jackType) list * jackFunctionType) TypeTable.t }

let new_class_type ty_vars =
    { type_variables = ty_vars
    ; variables = TypeTable.empty
    ; functions = TypeTable.empty
    }

let rec unzip l = match l with
    | [] -> ([],[])
    | (x, y) :: l -> let (xs, ys) = unzip l in (x :: xs, y :: ys)
let rec zip xs ys = match (xs, ys) with
    | ([], []) -> []
    | (x::xs, y::ys) -> (x,y)::(zip xs ys)
    | _ -> [] (* careful usage means we won't get here *)

let add_function table args ret_ty func_ty locals name = 
    let (arg_tys, arg_names) = unzip args in
    let args = zip arg_names arg_tys in
    let locals = List.concat_map (fun (ty, ids) -> List.map (fun id -> (id, ty)) ids) locals in
    { table with functions = TypeTable.add name (args, ret_ty, locals, func_ty) table.functions }

let add_variable table var_ty var_binding name = 
    { table with variables = TypeTable.add name (var_ty, var_binding) table.variables }

let get_variable table cls var = 
    TypeTable.find_opt var (TypeTable.find cls table).variables

let get_function table cls func = 
    TypeTable.find_opt func (TypeTable.find cls table).functions

type typeTable = classType TypeTable.t

