%{
    
%}

%token <string> ID 
%token <int> INT
%token <string> STR
%token WHILE FOR TO BREAK LET IN END FUNCTION VAR TYPE ARRAY IF THEN ELSE DO OF NIL

%token EOF
%token COMMA
%token COLON
%token SEMICOLON
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE 
%token DOT
%token PLUS MINUS
%token TIMES DIV
%token EQ NEQ LT LE GT GE
%token AND OR
%token ASSIGN

%nonassoc EQ NEQ 
%left PLUS MINUS
%left TIMES DIV

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