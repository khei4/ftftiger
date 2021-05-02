open OUnit2
open Syntax

let rec readloop lexbuf =
  let t = Lexer.token lexbuf in
  if t = EOF then [] else t :: readloop lexbuf

let lex str =
  let lexbuf = Lexing.from_string str in
  readloop lexbuf

let lex_from_file filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  readloop lexbuf

let rec print_tokenlist tokl =
  match tokl with
  | tok :: t -> print_token tok ^ "; " ^ print_tokenlist t
  | [] -> "\n"

and print_token tok =
  "Parser."
  ^
  match tok with
  | Parser.LET -> "LET"
  | Parser.TYPE -> "TYPE"
  | Parser.ID n -> "ID " ^ n
  | Parser.EQ -> "EQ"
  | Parser.OF -> "OF"
  | Parser.VAR -> "VAR"
  | Parser.COLON -> "COLON"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.LBRACKET -> "LBRACKET"
  | Parser.RBRACKET -> "RBRACKET"
  | Parser.INT v -> "INT " ^ string_of_int v
  | Parser.IN -> "IN"
  | Parser.END -> "END"
  | Parser.ARRAY -> "ARRAY"
  | _ -> "hoge"

let basic_lex_test name expected input =
  name >:: fun _ -> assert_equal expected (lex input) ~printer:print_tokenlist

let sample_lex_test name expected input =
  name >:: fun _ ->
  assert_equal expected (lex_from_file input) ~printer:print_tokenlist

let suite =
  "Basic"
  >::: [
         basic_lex_test "single_digit" [ Parser.INT 1 ] "1";
         basic_lex_test "multiple_digit"
           [ Parser.LPAREN; Parser.INT 1234; Parser.RPAREN ]
           "(1234)";
         basic_lex_test "field access"
           [ Parser.ID "obj"; Parser.LBRACKET; Parser.ID "fi"; Parser.RBRACKET ]
           "obj[fi]";
         sample_lex_test "test0"
           [ Parser.ID "hoge"; Parser.LBRACKET; Parser.ID "t"; Parser.RBRACKET ]
           "../samples/test0.tig";
         sample_lex_test "test1"
           [
             Parser.LET;
             Parser.TYPE;
             Parser.ID "arrtype";
             Parser.EQ;
             Parser.ARRAY;
             Parser.OF;
             Parser.ID "int";
             Parser.VAR;
             Parser.ID "arr1";
             Parser.COLON;
             Parser.ID "arrtype";
             Parser.ASSIGN;
             Parser.ID "arrtype";
             Parser.LBRACKET;
             Parser.INT 10;
             Parser.RBRACKET;
             Parser.OF;
             Parser.INT 0;
             Parser.IN;
             Parser.ID "arr1";
             Parser.END;
           ]
           "../samples/test1.tig";
         sample_lex_test "test2"
           [
             Parser.LET;
             Parser.TYPE;
             Parser.ID "myint";
             Parser.EQ;
             Parser.ID "int";
             Parser.TYPE;
             Parser.ID "arrtype";
             Parser.EQ;
             Parser.ARRAY;
             Parser.OF;
             Parser.ID "myint";
             Parser.VAR;
             Parser.ID "arr1";
             Parser.COLON;
             Parser.ID "arrtype";
             Parser.ASSIGN;
             Parser.ID "arrtype";
             Parser.LBRACKET;
             Parser.INT 10;
             Parser.RBRACKET;
             Parser.OF;
             Parser.INT 0;
             Parser.IN;
             Parser.ID "arr1";
             Parser.END;
           ]
           "../samples/test2.tig";
           sample_lex_test "test9: wrong if"
           [
             Parser.IF;
             Parser.LPAREN;
             Parser.INT 5;
             Parser.GT;
             Parser.INT 4;
             Parser.RPAREN;
             Parser.THEN;
             Parser.INT 13;
             Parser.ELSE;
             Parser.STR " ";
           ]
           "../samples/test9.tig";
       ]

let () = run_test_tt_main suite
