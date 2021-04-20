%{
    
%}

%token <string> ID 
%token <int> INT
%token <string> STR
%token WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF NIL


%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE 
%token DOT
%token PLUS
%token MINUS
%token TIMES
%token DIV
%token EQ
%token NEQ
%token LT
%token LE
%token GT
%token GE
%token AND
%token OR
%token ASS
%token EOF

%type <Ast.exp> program
%start program

%%

program: 
| exp EOF
  {$1}

exp: 
| INT
  {Ast.IntExp($1)}
| NIL
  {Ast.NilExp}
| unitexp
  {$1}
| lvalue
  {Ast.VarExp($1)}

unitexp: 
| LPAREN RPAREN
  {Ast.NilExp}

lvalue:
| ID 
  {Ast.SimpleVar($1, Ast.to_pos($startpos))}
| lvalue DOT ID 
  {Ast.FieldVar($1, $3, Ast.to_pos($startpos))}
| lvalue LBRACKET exp RBRACKET
  {Ast.SubscriptVar($1, $3, Ast.to_pos($startpos))}