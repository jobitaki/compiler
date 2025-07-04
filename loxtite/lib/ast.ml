type pos = {
  line: int;
  column: int;
} [@@deriving show]

type 'a located = {
  value: 'a;
  pos: pos;
} [@@deriving show]

type binop =
  | Add
  | Sub
  | Mul 
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  [@@deriving show]

type unop =
  | Not
  | Neg
  [@@deriving show]

type literal =
  | Float of float
  | String of string
  | Bool of bool
  | Null
  [@@deriving show]

type expr =
  | Literal of literal located
  | Identifier of string located 
  | BinaryOp of expr * binop located * expr
  | UnaryOp of unop located * expr
  | FunctionCall of string located * expr list
  [@@deriving show]

type stmt = 
  | Expression of expr
  | Assignment of string located * expr
  | VarDeclaration of string located * expr option
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | For of string located * expr * expr * expr * stmt list
  | Return of expr option (* What's an option? None or something *)
  (* | Break
  | Continue
  | Block of stmt list *)
  [@@deriving show]

type func_def = {
  name: string located;
  params: string located list;
  body: stmt list;
}
[@@deriving show]

type program = {
  functions: func_def list;
  main: stmt list;
}
[@@deriving show]

let make_located value pos = { value; pos }
let dummy_pos = { line = 0; column = 0 }
let make_dummy_located value = { value; pos = dummy_pos }