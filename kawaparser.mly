%{

  open Lexing
  open Kawa
%}

%token <int> N
%token <string> IDENT
%token FINAL CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID EXTENDS MAIN
%token ASSIGN LPAR RPAR L_BRACKET R_BRACKET BEGIN END SEMI COMMA DOT
%token PLUS MINUS STAR DIV MOD
%token EQUAL NEQUAL EQUAL_STRUCT NEQUAL_STRUCT LOWER LEQUAL GREATER GEQUAL AND OR NOT
%token PRINT TRUE FALSE
%token EOF

%left OR, AND
%left LOWER, LEQUAL, GREATER, GEQUAL, NEQUAL, EQUAL, EQUAL_STRUCT, NEQUAL_STRUCT
%left MINUS, PLUS
%left DIV, STAR, MOD
%left U_op
%left DOT

%start program
%type <Kawa.program> program

%%

program:
| glb=list(var_decl) cls=list(class_def) main_fun=main EOF { {classes=cls; globals= List.concat glb; main=main_fun} }
;

main:
| MAIN BEGIN seq=list(instr) END { seq }
;

class_def:
| CLASS cls_name=IDENT parent=option(class_extension) BEGIN attrs_methods=list(class_attr_meth) END { (*here we get a list of both methods and attributes and we split them back. This choice means there's no constraint as far as the order : an attribute can be after a method*)
                                                                                                      let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in 
                                                                                                      let attrs = List.filter_map (fun m_v -> match m_v with Attr (attr) -> Some attr | _ -> None) attrs_methods in
                                                                                                      {class_name=cls_name; attributes=List.concat attrs; methods=methods; parent=parent} 
                                                                                                     }
;

class_extension: /*class_extension needs to be its own nonterminal symbol to be used as an option in class_def*/
| EXTENDS parent=IDENT { parent }
;

class_attr_meth: 
| m=method_def { Method(m) }
| attr=attr_decl { Attr(attr) }

attr_decl:
| vars=typed_variables SEMI {List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=false}) vars} 
| FINAL vars=typed_variables SEMI {List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=true}) vars} 
;

method_def:
| ret=type_ id=IDENT LPAR params=separated_list(COMMA, typed_variable) RPAR BEGIN instr_var_decls=list(instr_var_decl) END { (*we use the same trick for variables and instructions in method_def as we did for attributes and methods in class_def*)
                                                                                                                             let loc = List.filter_map (fun v_i -> match v_i with Var_decl vars -> Some vars | _ -> None) instr_var_decls in
                                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                                             {method_name=id; code=code; params=params; locals=List.concat loc; return=ret}
                                                                                                                           }
;

typed_variable:
| typ=type_ id=IDENT { (id, typ) }
;


instr_var_decl:
| vars=var_decl { Var_decl(vars) }
| i=instr { Instr(i) }


var_decl:
| vars=typed_variables SEMI { vars } 
;

typed_variables: /*for example : int a, b, c;*/
| typ=type_ ids=separated_nonempty_list (COMMA, IDENT) { List.map (fun id -> (id, typ)) ids }
;


type_:
| BOOL { TBool }
| INT { TInt }
| VOID { TVoid } 
| cls_name=IDENT { TClass(cls_name) }
| t=type_ L_BRACKET R_BRACKET { TArray(t) }
;

instr:
| PRINT LPAR e=expr RPAR SEMI { Print(e) }
| mem=mem ASSIGN e=expr SEMI { Set(mem, e) }
| IF LPAR e=expr RPAR BEGIN seq1=list(instr) END ELSE BEGIN seq2=list(instr) END { If(e, seq1, seq2) }
| IF LPAR e=expr RPAR BEGIN seq1=list(instr) END { If(e, seq1, []) }
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
| L_BRACKET exprs=separated_list(COMMA, expr) R_BRACKET { Array(exprs) }
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
| EQUAL_STRUCT { Eq_struct }
| NEQUAL_STRUCT { Neq_struct }
;