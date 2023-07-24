type vmLabel = string
type vmSegment =
    | Local | Argument | This | That
    | Constant | Static | Pointer | Temp

type vmCommand =
    | Add | Sub | Neg
    | Eq | Gt | Lt
    | And | Or | Not
    | Label of vmLabel
    | Goto of vmLabel
    | IfGoto of vmLabel
    | Push of vmSegment * int
    | Pop of vmSegment * int
    | Function of string * int
    | Call of string * int
    | Return

type hackReg = D | A | M
type hackDest = hackReg list
type hackJump = JGT | JEQ | JGE | JLT | JNE | JLE | JMP
type hackUOp = PlusOne | MinusOne | Neg | Not
type hackBOp = Add | Sub | And | Or
type hackExpr = Zero | One | NegOne | UOp of hackUOp * hackReg
    | BOp of hackBOp * hackReg * hackReg (* one must be D *)

type hackCommand =
    | A of (int, string) Either.t
    | C of hackDest option * hackExpr * hackJump option
    | Label of string

let show_segment = function
    | Local -> "local" | Argument -> "argument"
    | This -> "this" | That -> "that"
    | Constant -> "constant" | Static -> "static"
    | Pointer -> "pointer" | Temp -> "temp"

let show_vm (vm:vmCommand) : string = match vm with
    | Add -> "    add" | Sub -> "    sub" | Neg -> "    neg"
    | Eq -> "    eq" | Gt -> "    gt" | Lt -> "    lt"
    | And -> "    and" | Or -> "    or" | Not -> "    not"
    | Label label -> "label " ^ label
    | Goto label -> "    goto " ^ label
    | IfGoto label -> "    if-goto " ^ label
    | Push (seg, index) -> "    push " ^ (show_segment seg) ^ " " ^ (string_of_int index)
    | Pop (seg, index) -> "    pop " ^ (show_segment seg) ^ " " ^ (string_of_int index)
    | Function (name, nLocals) -> "function " ^ name ^ " " ^ (string_of_int nLocals)
    | Call (name, nArgs) -> "    call " ^ name ^ " " ^ (string_of_int nArgs)
    | Return -> "    return"

