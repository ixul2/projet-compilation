%{

  open Lexing
  open Kawa

%}

%token <int> N
%token <string> IDENT
%token VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID EXTENDS MAIN
%token ASSIGN LPAR RPAR BEGIN END SEMI DOT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQUAL LOWER LEQUAL GREATER GEQUAL AND OR NOT
%token PRINT TRUE FALSE
%token EOF


%start program
%type <Kawa.program> program

%%

program:
| glb=list(var_decl) cls=list(class_def) main_fun=main EOF { {classes=cls; globals=glb; main_fun} }
;

class_def:
| CLASS cls_name=IDENT BEGIN attrs=list(attr_decl) methods=list(method_def) END {class_name=cls_name; attribute=attrs; methods=methods; parent=None}}
| CLASS cls_name=IDENT EXTENDS parent=IDENT BEGIN attrs=list(attr_decl) methods=list(method_def) END {class_name=cls_name; attribute=attrs; methods=methods; parent=Some parent}}
;

var_decl:
| VAR t=type_ id=IDENT SEMI { (t, id) } 

attr_decl:
| ATTRIBUTE t=type_ id=IDENT { (t, id) }

type_:
| BOOL { TBool }
| INT { TInt }
| VOID { TVoid } 
| cls_name=IDENT { TClass(cls_name) } 

method_def:
| METHOD ret=type_ id=IDENT BEGIN loc=list(var_decl) code=list(instr) END { {method_name=id; code=code; params=[]; locals=loc; return=ret} }
| METHOD ret=type_ id=IDENT LPAR params=list(type_ IDENT) RPAR BEGIN loc=list(var_decl) code=list(instr) END { {method_name=id; code=code; params=params; locals=loc; return=ret} }

expr:
| n=N { Int(n) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }
| THIS { This }
| m=mem { Get(m) }
| u=uop e=expr { Unop(u, e) }
| e1=expr b=bop e2=expr { Binop(e1, b, e2) }
| LPAR e=expr RPAR { e }
| NEW id=ident { New(id) }
| NEW id=IDENT LPAR params=list(expr) RPAR { NewCstr(id, params) }
| e=expr DOT id=IDENT LPAR params=list(expr) RPAR { MethCall(e, id, params) }
;

mem:
| var=IDENT { Var(var) }
| e=expr DOT var=IDENT { Field(e, var) }

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem=EQUAL e=expr SEMI { Set(mem, e) }
| IF LPAR e=expr RPAR BEGIN seq1=list(instr) END ELSE BEGIN seq2=list(instr) END { If(e, seq1, seq2) }
| WHILE LPAR e=expr RPAR BEGIN seq=list(instr) END { While(e, seq) }
| RETURN e=expr { Return(e) }
| e=expr { Expr(e) }
;

uop:
| MINUS { Opp }
| NOT { Not }

bop:
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
| MAIN BEGIN seq=list(instruction) END { seq }
