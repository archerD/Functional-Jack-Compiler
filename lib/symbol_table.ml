open Jack_ast
open Vm_ast
(* symbol table thing *)

type kind = 
    | Static
    | Field
    | Arg
    | Var

let kind_is_subroutine = function | Arg | Var -> true | _ -> false
let kind_is_class = function | Static | Field -> true | _ -> false

(* type symbolData = kind * int * string *)
type symbolData = { kind : kind ; index : int ; ty : jackType }

module SymbolMap = Map.Make(String)

type classTable =
    { genericTemplates : (string list * vmCommand list) SymbolMap.t
    ; genericsRequested : (string * string list) list
    (* ; types : TypeTable *)
    }

type symbolTable =
        { classScope : symbolData SymbolMap.t
        ; subroutineScope : symbolData SymbolMap.t
        ; staticIndex : int
        ; fieldIndex : int
        ; varIndex : int
        ; argIndex : int
        ; labelCount : int
        ; className : string
        ; tyVars : (string list) option
        ; otherClasses : classTable }

let new_table =
    { classScope = SymbolMap.empty
    ; subroutineScope = SymbolMap.empty
    ; staticIndex = 0 ; fieldIndex = 0
    ; varIndex = 0 ; argIndex = 0 
    ; labelCount = 0 ; className = ""
    ; tyVars = None
    ; otherClasses =
        { genericTemplates = SymbolMap.empty
        ; genericsRequested = []
        }
    }

(* table is last so it can easily be used with mutate_table and access_table *)
let reset_subroutine st = 
    { st with subroutineScope = SymbolMap.empty
            ; varIndex = 0 ; argIndex = 0 }

let reset_class cls_name ty_vars st =
    { (reset_subroutine st) with classScope = SymbolMap.empty
            ; staticIndex = 0; fieldIndex = 0; labelCount = 0
            ; className = cls_name ; tyVars = ty_vars}

let lookup_sym symbol table =
        let sub_val = SymbolMap.find_opt symbol table.subroutineScope in
        let cls_val = SymbolMap.find_opt symbol table.classScope in
        if Option.is_some sub_val then sub_val else cls_val

let get_class_name_base cls_name ty_vars = 
            String.concat "$" (cls_name :: List.flatten (Option.to_list ty_vars))
let get_class_name table : string = get_class_name_base table.className table.tyVars

let var_count table = function
        | Static -> table.staticIndex
        | Field -> table.fieldIndex
        | Arg -> table.argIndex
        | Var -> table.varIndex

let insert_generic_class cls_name ty_vars generic_template table = 
    { table with otherClasses = { table.otherClasses with genericTemplates = SymbolMap.add cls_name (ty_vars,generic_template) table.otherClasses.genericTemplates } }

let request_generic_class cls_name ty_params table =
    if List.mem (cls_name, ty_params) table.otherClasses.genericsRequested then table else
    { table with otherClasses = { table.otherClasses with genericsRequested = (cls_name, ty_params) :: table.otherClasses.genericsRequested } }

let instantiate_generic_class cls_name ty_args table = 
    let (ty_params, generic_template) = SymbolMap.find cls_name table.otherClasses.genericTemplates in
    let ty_param_to_arg = List.combine ty_params ty_args in
    (* this function will take a function call and update any type parameters in the name
     * The logic is as follows: extract the class name from the call, then split into the different parts (the base name and type variables)
     *  Then substitute each of the type variables with the given type argument *)
    let updateName name =
        let name_segments = String.split_on_char '.' name in (* the class name is the head of this list *)
        let class_parts = String.split_on_char '$' (List.hd name_segments) in
        let updated_class_parts = List.map (fun cp -> 
            if List.mem cp ty_params then List.assoc cp ty_param_to_arg else cp
            ) class_parts in
        let updated_class_name = String.concat "$" updated_class_parts in
        String.concat "." (updated_class_name :: (List.tl name_segments))
    in List.map (function 
        | Function (name, nLocals) -> Function (updateName name, nLocals)
        | Call (name, nArgs) -> Call (updateName name, nArgs)
        | vm -> vm) generic_template

let insert_sym k ty sym table = 
    let table = match ty with 
        | Generic (name, ty_params) -> request_generic_class name ty_params table
        | _ -> table
    in 
    match k with
    | Static -> { table with classScope = SymbolMap.add sym {kind = k; index = table.staticIndex; ty} table.classScope ; staticIndex = table.staticIndex + 1}
    | Field -> { table with classScope = SymbolMap.add sym {kind = k; index = table.fieldIndex; ty} table.classScope ; fieldIndex = table.fieldIndex + 1}
    | Arg -> { table with subroutineScope = SymbolMap.add sym {kind = k; index = table.argIndex; ty} table.subroutineScope ; argIndex = table.argIndex + 1}
    | Var -> { table with subroutineScope = SymbolMap.add sym {kind = k; index = table.varIndex; ty} table.subroutineScope ; varIndex = table.varIndex + 1}

