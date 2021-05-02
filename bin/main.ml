open Syntax

let string_of_pos (p : Ast.pos) =
  "{ bol = " ^ string_of_int p.bol ^ ", lnum = " ^ string_of_int p.lnum ^ "}"

let pprint (exp : Ast.exp) =
  let rec string_of_exp e =
    match e with
    | Ast.IntExp i -> string_of_int i
    | Ast.VarExp v -> string_of_var v
    | StringExp (s, p) -> "\"" ^ s ^ string_of_pos p ^ "\""
    | _ -> "not implemented"
  and string_of_var v =
    match v with
    | Ast.SimpleVar (s, p) -> s ^ string_of_pos p
    | Ast.SubscriptVar (v, e, _p) ->
        string_of_var v ^ "[" ^ string_of_exp e ^ "]"
    | Ast.FieldVar (v, field, _p) -> string_of_var v ^ field
  in
  print_endline (string_of_exp exp)

let _ =
  let filename = read_line () in
  let lexbuf =
    Lexing.from_channel (open_in ("./samples/" ^ filename ^ ".tig"))
  in
  let expr = Syntax.Parser.program Syntax.Lexer.token lexbuf in
  pprint expr
