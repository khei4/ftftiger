let _ = 
  while true do 
    let lexbuf = Lexing.from_channel stdin in 
    let expr = Syntax.Parser.program Syntax.Lexer.token lexbuf in Syntax.Util.pprint expr
  done 
  

