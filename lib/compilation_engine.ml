open Jack_ast
open Symbol_table
open Type_table
open Vm_ast
open Compilers


(* change string to char list, from stack overflow *)
let explode s = List.init (String.length s) (String.get s)

let kind_to_segment (k:kind) : vmSegment = match k with
        | Static -> Static | Field -> This
        | Arg -> Argument | Var -> Local

let rec string_of_type ty = match ty with
        | Int -> "int" | Char -> "char"
        | Bool -> "bool" | Class cls -> cls
        | Generic (base, ty_vars) -> String.concat "$" (base :: ty_vars)
        | FunctionTy (params, ret) -> "<(" ^ (String.concat "," (List.map string_of_type params)) ^ ") -> " ^ (Option.fold ~none:"void" ~some:(fun ret -> string_of_type ret) ret) ^ ">"
        | Top -> "TOP"


(* compilation engine *)
let rec compile_jack_expr expr : jcompiler = try
        let var_compiler var = try let+ {kind ; index ; _ } = lookup var
                                in [Push (kind_to_segment kind, index)]
                            with _ -> raise (CompilationError "nonexistant variable lookup in expression")
                in
    match expr with
        | IntConst i -> return [Push (Constant, i)]
        | StringConst s -> let l = String.length s in return @@
            [ Push (Constant, l) ; Call ("String.new", 1) ] @ List.concat_map
            (fun c -> [Push (Constant, Char.code c) ; Call ("String.appendChar", 2)]) (explode s) 
        | KeywordConst kw -> (match kw with
            | False | Null -> return [Push (Constant, 0)]
            | This -> return [Push (Pointer, 0)]
            | True -> return [Push (Constant, 1); Neg])
        | Var v -> var_compiler v
        | Array (v, e) -> var_compiler v <+> compile_jack_expr e <+> 
                            return [ Add ; Pop (Pointer, 1) ; Push (That, 0) ]
        | UOp (uop, e) -> compile_jack_expr e <+> return [match uop with Not -> Not | Neg -> Neg]
        | BinOp (e1, bop, e2) -> compile_jack_expr e1 <+> compile_jack_expr e2 <+> return 
                (match bop with Add -> [Add] | Minus -> [Sub] | And -> [And] | Or -> [Or]
                        | Equal -> [Eq] | Greater -> [Gt] | Less -> [Lt]
                        | Mult -> [Call ("Math.multiply", 2)]
                        | Div -> [Call ("Math.divide", 2)])
        | Lambda _ -> raise (CompilationError "Lambdas not supported yet")
        | Subroutine (rcvr, fname, args) -> compile_subroutine rcvr fname args
      with _ -> raise (CompilationError ("Error compiling expression " ^ show_jackExpression expr))

    and compile_argument_list args : jcompiler = 
        sum_over_list (compile_jack_expr) args

    and compile_subroutine rcvr fname args : jcompiler =
        (* TODO: clean up this code to reduce the use of Option.get, probably by altering how methods are handled
         * as it stands, all option.get calls should be fine since they happen after a check to is_method,
         * which can only be false if rcvr is Some s, and if true, either an implict this, or the rcvr is in the lookup table *)
        (* option.fold rcvr ~none ~some is like a switch on whether the rcvr is an implicit this, or something explicit
         * explicit this will be transformed into arg 0, while implicit this is pointer 0.  Both should be pointers to the same thing
         * is it worth doing rcvr = Option.value rcvr "this"? see commented code below.  It seems to be bugged... *)
        (* TODO: bug in constructors, where explicit rcvr 'this' is not correctly handled... but this is not present in test cases... *)
        let* cls = access_table (fun t -> t.className) in
        (* let rcvr = Option.value rcvr ~default:"this" in *)
        (* something is a method if the rcvr is a variable (implicit this 'is' a variable) *)
        let* is_method = Option.fold rcvr ~none:(return_a true) ~some:(fun rcvr -> lookup_opt rcvr >>| Option.is_some) in
        (* let* is_method = lookup_opt rcvr >>| (fun r -> Option.is_some r || String.equal rcvr "this") in *)
        let num_args = List.length args + (if is_method then 1 else 0) in
        let* rcvr_type = (if is_method
                then (Option.fold rcvr ~none:(return_a cls)
                        ~some:(fun rcvr -> (lookup rcvr >>| (fun r -> string_of_type r.ty))))
                else return_a @@ Option.get rcvr) in
                (* then (lookup_opt rcvr >>| Option.fold ~none:cls ~some:(fun r -> string_of_type r.ty)) *)
                (* else return_a rcvr) in *)
        let push_rcvr = (if is_method then 
            let* (seg, ind) = (
                Option.fold rcvr ~none:(return_a (Pointer, 0)) ~some:(fun rcvr ->
                    let+ rcvr_data = lookup rcvr in (kind_to_segment rcvr_data.kind, rcvr_data.index))
                (* (lookup_opt rcvr) >>| Option.fold ~none:(Pointer, 0) *)
                (*     ~some:(fun rcvr_data -> (kind_to_segment rcvr_data.kind, rcvr_data.index)) *)
            ) in return [Push (seg, ind)] else return []) in
        push_rcvr <+> compile_argument_list args <+> (return [Call (rcvr_type ^ "." ^ fname, num_args)])

let rec compile_jack_statement stat : jcompiler = try match stat with
        | Let (id, index, value) -> (Option.fold ~none:(return []) ~some:(fun ind -> compile_jack_expr ind) index)
            <+> compile_jack_expr value <+> if Option.is_some index (* TODO: there is nothing being done with index... *)
            then (lookup id >>| fun {kind; index; _} ->
                [Pop (Temp, 1); Push (kind_to_segment kind, index); Add; Pop (Pointer, 1); Push (Temp, 1); Pop (That, 0)])
            else (lookup id >>| fun {kind; index; _} -> [Pop (kind_to_segment kind, index)])
        | If (cond, th, el) -> 
                let* count = next_label_suffix in
                let if_end = "ifEnd" ^ count in
                let else_end = "elseEnd" ^ count in
                compile_jack_expr cond <+> return [Not; IfGoto if_end] <+> compile_statement_block th <+> 
                Option.fold el ~none:(return [Label if_end]) ~some:(fun el ->
                    return [Goto else_end; Label if_end] <+> compile_statement_block el <+> return [Label else_end])
        | While (cond, body) -> 
                let* count = next_label_suffix in
                let while_start = "whileStart" ^ count in
                let while_end   = "whileEnd" ^ count in
                return [Label while_start] <+> compile_jack_expr cond <+> return [Not; IfGoto while_end]
                <+> compile_statement_block body <+> return [Goto while_start; Label while_end]
        | Do (rcvr, fname, args) -> compile_subroutine rcvr fname args <+> return [Pop (Temp, 0)]
        | Return (Some ret_val) -> compile_jack_expr ret_val <+> return [Return]
        | Return None -> return [Push (Constant, 0); Return]
    with 
        _ -> raise (CompilationError ("Compilation error in statement '" ^ show_jackStatement stat ^ "'"))
    and compile_statement_block stats : jcompiler = match stats with
        | stat :: stats -> compile_jack_statement stat <+> compile_statement_block stats
        | [] -> return []

let compile_jack_param_list params : unit compiler =
        iterate_over_list (fun (ty, id) -> mutate_table @@ insert_sym Arg ty id) params

let compile_jack_local_vars vars : unit compiler =
        iterate_over_list (fun (ty, ids) ->
            iterate_over_list (fun id -> mutate_table @@ insert_sym Var ty id) ids) vars

let compile_jack_subroutine func : jcompiler =
        let (ft, _ret, name, params, locals, body) = func in
        let* () = mutate_table reset_subroutine in
        let* cls = access_table get_class_name in
        let* () = ite (ft == Method) (mutate_table @@ insert_sym Arg (Class cls) "this") (return_a ()) in
        let* () = compile_jack_param_list params in
        let* () = compile_jack_local_vars locals in
        let* num_locals = access_table (fun t -> var_count t Var) in
        let* num_fields = access_table (fun t -> var_count t Field) in
        return [Function (cls ^ "." ^ name, num_locals)]
        <+> (match ft with 
        | Constructor -> return
            [Push (Constant, num_fields); Call ("Memory.alloc", 1); Pop (Pointer, 0)]
        | Method -> return [Push (Argument, 0); Pop (Pointer, 0)]
        | Function -> return []
        ) <+> compile_statement_block body

let compile_jack_class_var (cvar : jackClassVar) : unit compiler = 
        let (binding, ty, ids) = cvar in
        let kind : kind = match binding with | Static -> Static | Field -> Field in
        iterate_over_list (fun id -> mutate_table @@ insert_sym kind ty id) ids

let compile_jack_class (cls: jackClass) : jcompiler = try
        let (name, ty_vars, cvars, subroutines) = cls in
        let* () = mutate_table (reset_class name ty_vars) in
        let* () = iterate_over_list compile_jack_class_var cvars in
        let* vm_funcs = sum_over_list compile_jack_subroutine subroutines in
        Option.fold ty_vars ~none:(return vm_funcs) ~some:(fun ty_vars ->
            mutate_table (insert_generic_class name ty_vars vm_funcs) >>| (fun _ -> []))
      with _ -> let (name, _,_,_) = cls in raise (CompilationError ("Error compiling class " ^ name))

let jack_compiler (jack_classes: jackClass list) : (string * vmCommand list) list =
    let jack_compiler_wrapper table cls =
        let (vms, table) = compile_jack_class cls table in
        let (name, ty_vars, _, _) = cls in
        (table, (get_class_name_base name ty_vars, vms)) in
    let (table, labeled_vms) = List.fold_left_map jack_compiler_wrapper new_table jack_classes in
    let instantiate_generics (cls, ty_args) =
        (get_class_name_base cls (Some ty_args), instantiate_generic_class cls ty_args table) in
    let labeled_vms = labeled_vms @ (List.map instantiate_generics table.otherClasses.genericsRequested) in
    labeled_vms

(* TODO: lambdas:
    The parser needs support for lambdas, which is annoying because of the mutual recursion introduced by a full lambda.
    Lambdas compile as follows:
        generate a closure: A class with fields for each of the variables the lambda uses from outside it's scope, a run method, and a new constructor.
                the run method is the code of the lambda function
                the new constructor takes the values of the variables and stores them in their fields.
        create a new value from the closure: call closure_name.new(...) with the desired environment variables.
    Need a new form to call lambdas...
        the new form compiles to compile the expression of the lambda, then call the run function of the lambda with the arguments passed.
            the alternative is to instead have lambdas be run as mylambda.run(...) instead of mylambda(...). probably easier
    *)
(* TODO: generics:
 *  Compilers can already generate code for a generic class, but will have the wrong types for the type variable. DONE
 *  What I need is to be able to go through and substitute for all the type variables with the desired type. DONE
 *  Also need the typechecker to enforce stuff.
 *  I also need to be able to instantiate the code when it is needed:
        scan other (nongeneric) classes, and when they use a generic class, 
        instantiate the generic class by going through the vm code and setting the type variables to the new classes 
            this also needs to be recursive, in case a generic class uses another generic class *)


(* type checking *)

let get_class_type (_, ty_vars, vars, funcs) : classType = 
    let cls_ty = new_class_type ty_vars in
    let var_tys = List.fold_left (fun cls_ty (binding, ty, ids) ->
        List.fold_left (fun cls_ty -> add_variable cls_ty ty binding) cls_ty ids) cls_ty vars in
    List.fold_left (fun cls_ty (ft, ret, name, params, locals, _) -> add_function cls_ty params ret ft locals name) var_tys funcs

let gather_types (classes : jackClass list) : typeTable =
    List.fold_left (fun table cls -> let (name, _, _, _) = cls in TypeTable.add name (get_class_type cls) table) TypeTable.empty classes

(* let is_subtype super sub = *)
(*     match super with *)
(*     | None -> true *)
(*     | Some sup_t -> match sub with *)
(*         | None -> false *)
(*         | Some sub_t -> match (sup_t, sub_t) with *)
(*             | (Int, Int) -> true *)
(*             | (Char, Char) -> true *)
(*             | (Bool, Bool) -> true *)

(* returns None if the typing fails *)
let rec type_expr type_table (locals: (string*jackType) list) cls expr : jackType = match expr with
    | IntConst _ -> Int
    | StringConst _ -> (Class "String")
    | KeywordConst k -> (match k with
        | True | False -> Bool
        | Null -> Top
        | This -> (Class cls))
    | Var v -> let l_ty = List.assoc_opt v locals in
                let c_ty = Option.map fst @@ get_variable type_table cls v in
                if Option.is_some l_ty then Option.get l_ty else (Option.get c_ty)
    | Array _ -> Top
    | Subroutine (rcvr, name, params) -> Option.get @@ type_subroutine type_table locals cls (rcvr,name,params)
    | UOp (uop, expr) -> let e_ty = type_expr type_table locals cls expr in
        (match (uop, e_ty) with (Not, Top) | (Not, Bool) -> Bool
                             | (Neg, Top) | (Neg, Int) -> Int
                             | _ -> raise (CompilationError "Bad types for unary operator"))
    | BinOp (e1, bop, e2) ->
            let ty1 = type_expr type_table locals cls e1 in
            let ty2 = type_expr type_table locals cls e2 in
                    let ty = match (ty1, ty2) with
                        | (Top, Top) -> Top
                        | (Top, t) -> t
                        | (t, Top) -> t
                        | (t1, t2) -> if type_eq t1 t2 then t1 else (raise (CompilationError "mismatched types")) in
                    (match bop with
                    | Equal -> Bool
                    | And | Or -> if type_eq ty Bool then Bool else (raise (CompilationError "bad types for binar operator"))
                    | Greater | Less -> if type_eq ty Int then Bool else (raise (CompilationError "bad types for binar operator"))
                    | Add | Minus | Mult | Div -> if type_eq ty Int then Int else (raise (CompilationError "bad types for binar operator")))
    | Lambda (params, ret_ty, _, _) -> FunctionTy (fst @@ unzip params, ret_ty)
and type_subroutine type_table _locals cls (_rcvr,name,_params) : jackType option = 
    let (_args, ret_ty, _locals, _func_ty) = Option.get @@ get_function type_table cls name in
    ret_ty (* need to typecheck the parameters *)

let type_check_statement type_table locals cls fname stmt : bool = match stmt with
    | Do (rcvr, fname, args) -> let _ = type_subroutine type_table locals cls (rcvr,fname,args) in true
    | Return None -> let (_,ret,_,_) = Option.get @@ get_function type_table cls fname in Option.is_none ret
    | Return (Some expr) -> let (_,ret,_,_) = Option.get @@ get_function type_table cls fname in
                            let ty = type_expr type_table locals cls expr in
                            Option.equal (type_eq) ret (Some ty)
    | _ -> true (* typecheck lets (index is int and expr types, or no index, and expr matches var type), ifs (condition is bool, both branches type), and whiles (condition is bool, body types) *)


let type_check_function type_table cname func =
    let (_ft, _ret, fname, _params, _locals, body) = func in
    let (_args, _ret_ty, locals, _func_ty) = Option.get @@ get_function type_table cname fname in
    List.for_all (type_check_statement type_table locals cname fname) body

let check_types type_table cls =
    let (name, _ty_vars, _vars, funcs) = cls in
    List.for_all (type_check_function type_table name) funcs


