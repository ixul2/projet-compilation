%{

  open Lexing
  open Kawa

%}

%token <int> N
%token <string> IDENT
%token VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID EXTENDS MAIN
%token ASSIGN LPAR RPAR BEGIN END SEMI COMMA DOT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQUAL LOWER LEQUAL GREATER GEQUAL AND OR NOT
%token PRINT TRUE FALSE
%token EOF

%left OR, AND
%left LOWER, LEQUAL, GREATER, GEQUAL, NEQUAL, EQUAL
%left MINUS, PLUS
%left DIV, STAR, MOD
%left U_op
%left DOT

%start program
%type <Kawa.program> program

%%

program:
| glb=list(var_decl) cls=list(class_def) main_fun=main EOF { {classes=cls; globals=glb; main=main_fun} }
;

class_def:
| CLASS cls_name=IDENT BEGIN attrs=list(attr_decl) methods=list(method_def) END { {class_name=cls_name; attributes=attrs; methods=methods; parent=None} }
| CLASS cls_name=IDENT EXTENDS parent=IDENT BEGIN attrs=list(attr_decl) methods=list(method_def) END { {class_name=cls_name; attributes=attrs; methods=methods; parent=Some parent} }
;

var_decl:
| VAR t=type_ id=IDENT SEMI { (id, t) } 

attr_decl:
| ATTRIBUTE t=type_ id=IDENT SEMI { (id, t) }

type_:
| BOOL { TBool }
| INT { TInt }
| VOID { TVoid } 
| cls_name=IDENT { TClass(cls_name) } 

method_def:
| METHOD ret=type_ id=IDENT LPAR params=separated_list(COMMA, typed_variable) RPAR BEGIN loc=list(var_decl) code=list(instr) END { {method_name=id; code=code; params=params; locals=loc; return=ret} }

typed_variable:
| typ =type_ id=IDENT { (id, typ)}

expr:
| n=N { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| m=mem { Get(m) }
| u=uop e=expr { Unop(u, e) } %prec U_op
| e1=expr b=bop e2=expr { Binop(b, e1, e2) }
| LPAR e=expr RPAR { e }
| NEW id=IDENT { New(id) }
| NEW id=IDENT LPAR params=separated_list(COMMA, expr) RPAR { NewCstr(id, params) }
| e=expr DOT id=IDENT LPAR params=separated_list(COMMA, expr) RPAR { MethCall(e, id, params) }
;

mem:
| var=IDENT { Var(var) }
| e=expr DOT var=IDENT { Field(e, var) }

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem=mem ASSIGN e=expr SEMI { Set(mem, e) }
| IF LPAR e=expr RPAR BEGIN seq1=list(instr) END ELSE BEGIN seq2=list(instr) END { If(e, seq1, seq2) }
| WHILE LPAR e=expr RPAR BEGIN seq=list(instr) END { While(e, seq) }
| RETURN e=expr SEMI { Return(e) }
| e=expr SEMI { Expr(e) }
;

%inline uop: 
| MINUS { Opp }
| NOT { Not }

%inline bop:
| PLUS { Add }
| MINUS { Sub }
| STAR { Mul }
| DIV { Div }
| MOD { Rem }
| EQUAL { Eq }
| NEQUAL { Neq }
| LOWER { Lt }
| LEQUAL { Le }
| GREATER { Gt }
| GEQUAL { Ge }
| AND { And }
| OR { Or }

main:
| MAIN BEGIN seq=list(instr) END { seq }