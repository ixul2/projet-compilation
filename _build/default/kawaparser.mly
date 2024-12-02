%{

  open Lexing
  open Kawa

%}

%token <int> N
%token <string> IDENT
%token MAIN
%token VAR ATTRIBUTE METHOD CLASS NEW THIS IF ELSE WHILE RETURN INT BOOL VOID 
%token ASSIGN LPAR RPAR BEGIN END SEMI
%token PLUS MINUS STAR DIV  
%token EQUAL NEQUAL LOWER LEQUAL AND OR
%token PRINT TRUE FALSE
%token EOF


%start program
%type <Kawa.program> program

%%

program:
| MAIN BEGIN main=list(instruction) END EOF
    { {classes=[]; globals=[]; main} }
;

instruction:
| PRINT LPAR e=expression RPAR SEMI { Print(e) }
;

expression:
| n=N { Int(n) }
;
