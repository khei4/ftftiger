%{
    let to_pos (p: Lexing.position) : Ast.pos = {lnum = p.pos_lnum; bol = p.pos_bol}
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

%nonassoc DO THEN OF 
%nonassoc ELSE
%nonassoc ASSIGN

%left OR
%left AND
%nonassoc EQ NEQ LT GT GE LE 
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%type <Ast.exp> program
%start program

%%

program: 
| exp EOF
  {$1}

exp: 
| NIL
  {Ast.NilExp}
| IF exp THEN exp
  {Ast.IfExp {test = $2; then'= $4; else' = None; pos = to_pos($startpos)}}
| IF exp THEN exp ELSE exp
  {Ast.IfExp {test = $2; then'= $4; else' = Some $6; pos = to_pos($startpos)}}
| WHILE exp DO exp 
  {Ast.WhileExp {test = $2; body = $4; pos = to_pos($startpos)}}
// temporary escape to ref false
| FOR ID ASSIGN exp TO exp DO exp
  {Ast.ForExp {var = $2;escape = ref false;lo = $4; hi = $6; body = $8; pos = to_pos($startpos)}}
| BREAK
  {Ast.BreakExp (to_pos($startpos))}
| LPAREN separated_list(SEMICOLON, exppos) RPAREN
  {Ast.SeqExp $2}
| INT
  {Ast.IntExp($1)}
| MINUS exp %prec UMINUS
  {Ast.OpExp { left=Ast.IntExp 0; oper=Ast.MinusOp; right=$2; pos=to_pos($startpos)}}
| exp binop exp
  {Ast.OpExp {left=$1; oper= $2; right=$3; pos=to_pos($startpos)}}
| STR 
  {Ast.StringExp($1, to_pos($startpos))}
| lvalue
  {Ast.VarExp($1)}
| ID LPAREN separated_list(COMMA, exp) RPAREN
  {Ast.CallExp {func=$1; args=$3; pos=to_pos($startpos)}}
| ID LBRACE separated_list(COMMA, field) RBRACE 
  {Ast.RecordExp {fields=$3; typ=$1; pos=to_pos($startpos)}}
// | LET list(dec) IN separated_list(SEMICOLON, exppos) END 
//   {Ast.LetExp {decs=$2; body=(Ast.SeqExp $4); pos=to_pos($startpos)}}
| LPAREN RPAREN
  {Ast.NilExp}


// dec:
// | vardec 
//   {$1}
// | separated_nonempty_list(AND, fundec)
//   {Ast.FunctionDec fs}
// | separated_nonempty_list(AND, tydec)
//   {Ast.TypeDec $1}

// vardec:
// | VAR ID type_constraint ASSIGN exp 
//   {Ast.VarDec { name=$2; escape=ref false;typ=$3; init=$5; pos=to_pos($startpos)}}

// type_constraint:
// | option(COLON ID { ($2, to_pos($startpos))})
//   {$1}

// fundec:
// | FUNCTION ID LPAREN separated_list(COMMA, tyfield) RPAREN type_constraint EQ exp 
//   { { funname=$2; params=$4; result=$6; body=$8; funpos=to_pos($startpos) } }

// tyfield:
// | ID COLON ID 
//   { {name=$1; escape=ref false; typ=$3; pos=to_pos($startpos)}}

// tydec:
// | TYPE ID EQ ty
//   { {name=$2; ty=$4; pos=to_pos($startpos)}}

// ty:
// | ID
//   {NameTy($1, to_pos($startpos))}
// | LBRACE separated_list(COMMA, tyfield) RBRACE
//   { RecordTy fields }
// | ARRAY OF ID 
//   { ArrayTy($3, to_pos($startpos))}

lvalue:
| ID 
  {Ast.SimpleVar($1, to_pos($startpos))}
| lvalue DOT ID 
  {Ast.FieldVar($1, $3, to_pos($startpos))}
| lvalue LBRACKET exp RBRACKET
  {Ast.SubscriptVar($1, $3, to_pos($startpos))}

field : 
| ID EQ exp 
  {($1, $3, to_pos($startpos))}

exppos:
| exp 
  { ($1, to_pos($startpos)) }

%inline binop:
  | PLUS {Ast.PlusOp}
  | MINUS {Ast.MinusOp}
  | TIMES {Ast.TimesOp}
  | DIV {Ast.DivideOp}
  | EQ {Ast.EqOp}
  | NEQ {Ast.NeqOp}
  | LT {Ast.LtOp}
  | LE {Ast.LeOp}
  | GT {Ast.GtOp}
  | GE {Ast.GeOp}
  | AND {Ast.AndOp}
  | OR {Ast.OrOp}