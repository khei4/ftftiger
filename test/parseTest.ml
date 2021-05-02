open OUnit2
open Syntax
open Syntax.Ast

let parse p : exp =
  let lexbuf = Lexing.from_string p in
  Parser.program Lexer.token lexbuf

let parse_from_file s : exp =
  let lexbuf = Lexing.from_channel (open_in s) in
  Parser.program Lexer.token lexbuf

let basic_parse_test name expected input =
  name >:: fun _ ->
  assert_equal expected (parse input) ~printer:show_exp ~cmp:equal_exp

let sample_parse_test name expected filename =
  name >:: fun _ ->
  assert_equal expected (parse_from_file filename) ~printer:show_exp
    ~cmp:equal_exp

let suite =
  "Basic"
  >::: [
         basic_parse_test "single_digit" (IntExp 1) "1";
         basic_parse_test "multiple_digit" (IntExp 1234) "1234";
         basic_parse_test "field access"
           (VarExp
              (SubscriptVar
                 ( SimpleVar ("obj", { lnum = 1; bol = 0 }),
                   VarExp (SimpleVar ("fi", { lnum = 1; bol = 0 })),
                   { lnum = 1; bol = 0 } )))
           "obj[fi]";
         sample_parse_test "if expression"
           (Ast.IfExp
              {
                test =
                  Ast.SeqExp
                    [
                      ( Ast.OpExp
                          {
                            left = Ast.IntExp 10;
                            oper = Ast.GtOp;
                            right = Ast.IntExp 20;
                            pos = { Ast.lnum = 2; bol = 17 };
                          },
                        { Ast.lnum = 2; bol = 17 } );
                    ];
                then' = Ast.IntExp 30;
                else' = Some (Ast.IntExp 40);
                pos = { Ast.lnum = 2; bol = 17 };
              })
           "../samples/test8.tig";
         sample_parse_test "wrong if expression"
           (Ast.IfExp
              {
                test =
                  Ast.SeqExp
                    [
                      ( Ast.OpExp
                          {
                            left = Ast.IntExp 5;
                            oper = Ast.GtOp;
                            right = Ast.IntExp 4;
                            pos = { Ast.lnum = 3; bol = 43 };
                          },
                        { Ast.lnum = 3; bol = 43 } );
                    ];
                then' = Ast.IntExp 13;
                else' = Some (Ast.StringExp (" ", { Ast.lnum = 3; bol = 43 }));
                pos = { Ast.lnum = 3; bol = 43 };
              })
           "../samples/test9.tig";
         sample_parse_test "while expression"
           (Ast.WhileExp
              {
                test =
                  Ast.SeqExp
                    [
                      ( Ast.OpExp
                          {
                            left = Ast.IntExp 10;
                            oper = Ast.GtOp;
                            right = Ast.IntExp 5;
                            pos = { Ast.lnum = 2; bol = 37 };
                          },
                        { Ast.lnum = 2; bol = 37 } );
                    ];
                body =
                  Ast.OpExp
                    {
                      left = Ast.IntExp 5;
                      oper = Ast.PlusOp;
                      right = Ast.IntExp 6;
                      pos = { Ast.lnum = 2; bol = 37 };
                    };
                pos = { Ast.lnum = 2; bol = 37 };
              })
           "../samples/test10.tig";
       ]

let () = run_test_tt_main suite
