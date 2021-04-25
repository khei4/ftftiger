{
    open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z''a'-'z''_']
let ident = alpha (digit|alpha)*

rule token = parse 
| "while"
    { WHILE }
| "for"
    { FOR }
| "to"
    { TO }
| "break"
    { BREAK }
| "let"
    { LET }
| "in"
    { IN }
| "end"
    { END }
| "function"
    { FUNCTION }
| "var"
    { VAR }
| "type"
    { TYPE }
| "array"
    { ARRAY }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "do"
    { DO }
| "of"
    { OF }
| "nil"
    { NIL }
| ident as id 
    {ID(id)}
| digit+
    {INT(int_of_string (Lexing.lexeme lexbuf))}
| ','
    {COMMA}
| ':'
    {COLON}
| ';'
    {SEMICOLON}
| '('
    {LPAREN}
| ')'
    {RPAREN}
| '['
    {LBRACKET}
| ']'
    {RBRACKET}
| '{'
    {LBRACE}
| '}'
    {RBRACE}
| '.'
    {DOT}
| '+'
    {PLUS}
| '-'
    {MINUS}
| '*'
    {TIMES}
| '/'
    {DIV}
| '='
    {EQ}
| "<>"
    {NEQ}
| '<'
    {LT}
| "<="
    {LE}
| '>'
    {GT}
| ">="
    {GE}
| '&'
    {AND}
| '|'
    {OR}
| ":="
    {ASSIGN}
| eof 
    {EOF}
| space+
    {token lexbuf}

