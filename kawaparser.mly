%{

  open Lexing
  open Kawa

%}

%token <int> N
%token <string> IDENT
%token CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID EXTENDS MAIN
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

main:
| MAIN BEGIN seq=list(instr) END { seq }
;

class_def:
| CLASS cls_name=IDENT parent=option(class_extension) BEGIN attrs_methods=list(class_attr_meth) END { let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in
                                                                       let attrs = List.filter_map (fun m_v -> match m_v with Attr (id, t) -> Some (id, t) | _ -> None) attrs_methods in
                                                                       {class_name=cls_name; attributes=attrs; methods=methods; parent=parent} 
                                                                     }
;

class_extension:
| EXTENDS parent=IDENT { parent }
;

class_attr_meth:
| m=method_def { Method(m) }
| v_t=var_decl { let v, t = v_t in Attr(v, t) }

method_def:
| var=typed_variable LPAR params=separated_list(COMMA, typed_variable) RPAR BEGIN instr_var_decls=list(instr_var_decl) END { let loc = List.filter_map (fun v_i -> match v_i with Var_decl (v, t) -> Some(v, t) | _ -> None) instr_var_decls in
                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                             let id, ret = var in 
                                                                                                             {method_name=id; code=code; params=params; locals=loc; return=ret}
                                                                                                           }
;

instr_var_decl:
| v_t=var_decl { let v, t = v_t in Var_decl(v, t) }
| i=instr { Instr(i) }

var_decl:
| var=typed_variable SEMI { var } 
;

typed_variable:
| typ=type_ id=IDENT { (id, typ) }
;

type_:
| BOOL { TBool }
| INT { TInt }
| VOID { TVoid } 
| cls_name=IDENT { TClass(cls_name) }
;

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem=mem ASSIGN e=expr SEMI { Set(mem, e) }
| IF LPAR e=expr RPAR BEGIN seq1=list(instr) END ELSE BEGIN seq2=list(instr) END { If(e, seq1, seq2) }
| WHILE LPAR e=expr RPAR BEGIN seq=list(instr) END { While(e, seq) }
| RETURN e=expr SEMI { Return(e) }
| e=expr SEMI { Expr(e) }
;

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
;

%inline uop: 
| MINUS { Opp }
| NOT { Not }
;

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
;