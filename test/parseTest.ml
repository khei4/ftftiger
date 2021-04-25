open OUnit2
open Syntax
open Syntax.Ast



let parse p : exp =
  let lexbuf = Lexing.from_string p in
  Parser.program Lexer.token lexbuf

let basic_parse_test name expected input =
  name >:: fun _ -> assert_equal expected (parse input) ~printer:show_exp ~cmp:equal_exp


let suite =
  "Basic"
  >::: [
         basic_parse_test "single_digit" (IntExp 12) "1";
         basic_parse_test "multiple_digit" (IntExp 1234) "1234";
       ]

let () = run_test_tt_main suite