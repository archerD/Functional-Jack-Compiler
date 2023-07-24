open Angstrom
open Jack_ast

let is_whitespace = function (* taken from somewhere on the internet... *)
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true (* space, new line, carriage return, tab *)
  | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let whitespace = satisfy is_whitespace *> return ()
let _alpha = satisfy is_alpha
let _digit = satisfy is_digit
(* many and many1 are useful combinators *)

let comment : unit t =
           ((string "//" *> many_till any_char end_of_line)
        <|> (string "/*" *> many_till any_char (string "*/")))
        *> return ()
        (* >>| (fun _ -> ()) *)
        (* >>| fun chars -> (String.of_seq (List.to_seq chars)) *)

let lex_pre p = (many (whitespace <|> comment)) *> p
let lex p = p <* (many (whitespace <|> comment))

let _chainl1 e op = (* taken from angstrom tutorial *)
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let _is_num : string -> bool = String.for_all is_digit

let parens p  = (lex @@ char '(') *> p <* (lex @@ char ')')
let curlies p = (lex @@ char '{') *> p <* (lex @@ char '}')
let squares p = (lex @@ char '[') *> p <* (lex @@ char ']')
let angles p  = (lex @@ char '<') *> p <* (lex @@ char '>')

let optional p = option None (p >>| (fun a -> Some a))

let sym c v = lex @@ char c *> return v
let keyword s v = lex @@ string s *> return v
let jackOp_parser: jackOp t = choice
    [ sym '+' Add
    ; sym '-' Minus
    ; sym '*' Mult
    ; sym '/' Div
    ; sym '&' And
    ; sym '|' Or
    ; sym '=' Equal
    ; sym '<' Less
    ; sym '>' Greater
    ]
let jackUOp_parser = sym '-' Neg <|> sym '~' Not
let jackKeyword_parser = choice
    [ keyword "true" True
    ; keyword "false" False
    ; keyword "null" Null
    ; keyword "this" This
    ]

let _keywords = ["class";"constructor";"function";"method";"field";"static";"var";"int";"char";"boolean";"void";"true";"false";"null";"this";"let";"do";"if";"else";"while";"return"]
let identifier = lex @@ lift2 (fun c s -> (String.make 1 c) ^ s)
    (satisfy (fun c -> (is_alpha c) || c == '_'))
    (take_while (fun c -> (is_alpha c) || (is_digit c) || c == '_'))
    (* >>= (fun id -> if List.mem id _keywords then fail "Keywords are not identifiers" *)
    (*                                         else return id) *)

let integer = lex @@
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| (fun s -> (int_of_string s))

let string_const = lex @@ char '"' *> take_till (fun c -> c == '"') <* char '"'

let type_parser = fix (fun ty -> lex @@ choice 
        [ keyword "int" Int
        ; keyword "char" Char
        ; keyword "boolean" Bool
        ; angles (
            let* params = (parens @@ sep_by1 (sym ',' "") ty) in
            let* _ = keyword "->" "" in
            let* ret = keyword "void" None <|> (ty >>| (fun t -> Some t))  in
                return (FunctionTy (params, ret)) )
        ; (let* id = identifier in
           let* ty_vars = angles @@ sep_by1 (sym ',' "") identifier in
            return (Generic (id, ty_vars)) )
        ; identifier >>| fun n -> Class n ] )


let subroutine_parser param_parser = 
        let param_list = sep_by (sym ',' "") param_parser in
            lex @@ lift2 (fun fn es -> (None, fn, es)) identifier (parens param_list)
            <|> lift3 (fun pre fn es -> (Some pre, fn, es)) (identifier <* sym '.' "") identifier (parens param_list)

let jackExpr_parser : jackExpression t = fix (fun expr ->
        let term = fix (fun term -> lex @@ choice 
            [ integer >>| (fun i -> IntConst i)
            ; string_const >>| (fun s -> StringConst s)
            ; jackKeyword_parser >>| (fun k -> KeywordConst k)
            (* ; keyword "lambda" (fun params ret (vars, body) -> Lambda (params, ret, vars, body)) <*> *)
            (*     (parens (sep_by (sym ',' "") (both type_parser identifier))) <* (sym ':' "") <*> type_parser <*> *)
            (*     curlies (both *)
            (*         (many (keyword "var" "" *> both type_parser (sep_by1 (sym ',' "") identifier) <* sym ';' "")) *)
            (*         (many jackStatement_parser)) *)
            ; lift2 (fun i e -> Array (i,e)) identifier (squares expr)
            ; subroutine_parser expr >>| (fun (pre, name, es) -> Subroutine (pre, name, es))
            ; identifier >>| (fun v -> Var v)
            ; parens expr
            ; lift2 (fun uop t -> UOp (uop, t)) jackUOp_parser term
            ]) in
        let op_terms = many @@ both jackOp_parser term in
        let comb lt (op, rt) = BinOp (lt, op, rt) in 
        lex @@ lift2 (List.fold_left comb) term op_terms
    )

let jackStatement_parser = fix (fun statement ->
    let letParser = keyword "let" (fun var arr exp -> Let (var, arr, exp)) <*> identifier <*> optional (squares jackExpr_parser) <* sym '=' "" <*> jackExpr_parser <* sym ';' "" in
        let ifParser = keyword "if" (fun cond body elseBody -> If (cond, body, elseBody)) <*>
                parens jackExpr_parser <*> curlies (many statement) <*> optional (keyword "else" "" *> curlies (many statement)) in
        let whileParser = keyword "while" (fun cond body -> While (cond, body)) <*> parens jackExpr_parser <*> curlies (many statement) in
        let doParser = keyword "do" (fun (pre, fn, es) -> Do (pre, fn, es)) <*> subroutine_parser jackExpr_parser <* sym ';' "" in
        let returnParser = keyword "return" (fun expr -> Return expr) <*> optional jackExpr_parser <* sym ';' "" in
        lex @@ choice
            [ letParser 
            ; ifParser
            ; whileParser
            ; doParser
            ; returnParser
            ]
    )

let classVar_parser = lex @@
    let comb = fun bind varType names -> (bind, varType, names) in
    (keyword "static" (comb Static) <|> keyword "field" (comb Field)) <*>
    type_parser <*> sep_by1 (sym ',' "") identifier <* sym ';' ""

let function_parser = lex @@
    let comb = fun funType retType name params (vars, body) -> (funType, retType, name, params, vars, body) in
    (keyword "constructor" (comb Constructor) <|> keyword "function" (comb Function) <|> keyword "method" (comb Method))
    <*> (keyword "void" None <|> (type_parser >>| (fun t -> Some t)))
    <*> identifier <*> parens (sep_by (sym ',' "") (both type_parser identifier)) (* argument list *)
    <*> curlies (both (many (keyword "var" "" *> both type_parser (sep_by1 (sym ',' "") identifier) <* sym ';' ""))
         (many jackStatement_parser))

let jackProgram_parser : jackClass t = lex_pre @@ keyword "class" (fun name ty_vars (vars, funcs) -> (name, ty_vars, vars, funcs)) <*> identifier <*> (optional (angles @@ sep_by1 (sym ',' "") identifier)) <*> curlies (both (many classVar_parser) (many function_parser))
    
let evalJP (str:string) : jackClass =
  match parse_string ~consume:All jackProgram_parser str with
  | Ok v      -> v
  | Error msg -> failwith msg

