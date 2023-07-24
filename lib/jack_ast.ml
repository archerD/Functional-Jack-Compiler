type jackOp = Add | Minus | Mult | Div | And | Or | Equal | Greater | Less [@@deriving show]
type jackUOp = Not | Neg [@@deriving show]
type jackKeyword = True | False | Null | This [@@deriving show]

type jackType = Int | Char | Bool | Class of string (* 'basic' types *)
        | Generic of string * (string list) | FunctionTy of (jackType list) * jackType option (* 'new' types *)
        (* genericType<Type1, Type2> (types must be classes or primatives), <(paramTy1, ) -> returnTy> *)
        | Top
        [@@deriving show]
let type_eq t1 t2 = match (t1, t2) with
    | (Top, Top) | (Int, Int) | (Char, Char) | (Bool, Bool) -> true
    | (Class c1, Class c2) -> String.equal c1 c2
    | (Generic (c1, vs1), Generic (c2, vs2)) -> String.equal c1 c2 && (List.length vs1) = (List.length vs2)
    | (FunctionTy (_params1, _ret1), FunctionTy (_params2, _ret2)) -> false (* TODO: fix this, but this function probably won't be called on functions anyways*)
    | _ -> false

type jackExpression =
  | IntConst of int
  | StringConst of string
  | KeywordConst of jackKeyword
  | Var of string
  | Array of string * jackExpression
  | Subroutine of string option * string * jackExpression list
  | UOp of jackUOp * jackExpression
  | BinOp of jackExpression * jackOp * jackExpression
  (* lambdas (lambda (parameters) : type {body}) *)
  | Lambda of (jackType * string) list * jackType option * (jackType * string list) list * jackStatements
  [@@deriving show]

and jackStatement =
  | Let of string * jackExpression option * jackExpression
  | If of jackExpression * jackStatements * jackStatements option
  | While of jackExpression * jackStatements
  | Do of string option * string * jackExpression list
  | Return of jackExpression option [@@deriving show]

and jackStatements = jackStatement list

type jackBinding = Static | Field [@@deriving show]
type jackFunctionType = Constructor | Function | Method [@@deriving show]
type jackClassVar = jackBinding * jackType * string list [@@deriving show]

type jackSubroutine =
  jackFunctionType
  * jackType option
  * string
  * (jackType * string) list (* param list *)
  * (jackType * string list) list (* local var list *)
  * jackStatements [@@deriving show] (* body *)

  (* class name, generic type variables (<T>), variables, subroutines *)
type jackClass = string * (string list option) * jackClassVar list * jackSubroutine list [@@deriving show]
