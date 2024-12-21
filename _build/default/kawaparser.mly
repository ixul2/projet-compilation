%{

  open Lexing
  open Kawa

%}

%token <int> N
%token <string> IDENT
%token VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID EXTENDS
%token ASSIGN LPAR RPAR BEGIN END SEMI
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQUAL LOWER LEQUAL GREATER GEQUAL AND OR NOT
%token PRINT TRUE FALSE
%token EOF


%start program
%type <Kawa.program> program

%%

program:
| list(var_decl) list(class_def) main EOF { {classes=[]; globals=[]; main} }
;

class_def:
| CLASS cls_name=IDENT attr_decl { {class_name=cls_name; }}
| CLASS IDENT attr_decl EXTENDS IDENT
;

var_decl:
| VAR t=type_ IDENT SEMI {{}}

attr_decl:
| ATTRIBUTE type_ IDENT {} SEMI BEGIN list(attr_decl) list(method_def) END  

type_:
| BOOL { TBool }
| INT { TInt }
| VOID { TVoid } 
| cls_name=IDENT { TClass(cls_name) } 

method_def:
| METHOD type_ IDENT BEGIN list(var_decl) list(instr) END
| METHOD type_ IDENT LPAR list(type_ IDENT) RPAR BEGIN list(var_decl) list(instr) END

expr:
| n=N { Int(n) }
| TRUE
| FALSE
| THIS
| mem
| uop expr
| expr bop expr
| LPAR expr RPAR
| NEW ident
| NEW ident LPAR list(expr) RPAR
| expr . IDENT LPAR list(expr) RPAR 
;

mem:
| IDENT
| expr . IDENT

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
| list(instruction)
