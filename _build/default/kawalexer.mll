{

  open Lexing
  open Kawaparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "print",      PRINT;
      "main",       MAIN;
      "true",       TRUE;
      "false",      FALSE;

<<<<<<< HEAD
=======
      "var",        VAR;
      "attribute",  ATTRIBUTE;
      "final",      FINAL;
      "method",     METHOD;
>>>>>>> 541246b (idk anymore)
      "class",      CLASS;
      "new",        NEW;
      "this",       THIS;
      "if",         IF;
      "else",       ELSE;
      "while",      WHILE;
      "extends",    EXTENDS;
      "return",     RETURN;
      "int",        INT;
      "bool",       BOOL;
      "void",       VOID;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)  
        
}

let digit = ['0'-'9']
let number = digit+ (*removed "['-']?" instead '- is handled as a unary operator by the grammar. Otherwise 2-2 is misinterpreted as Number Number by the lexer*)
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*
  
rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "//" [^ '\n']* "\n"  { new_line lexbuf; token lexbuf }
  | "/*"                 { comment lexbuf; token lexbuf }

  | number as n  { N(int_of_string n) }
  | ident as id  { match (List.find_opt (fun x -> x=id) ["var"; "attribute"; "method"]) with
                     None -> keyword_or_ident id
                   | _ -> token lexbuf }
  
   
  | "="  { ASSIGN }
  | ";"  { SEMI }
  | "."  { DOT }
  | ","  { COMMA }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { STAR }
  | "/"  { DIV }
  | "%"  { MOD }
  | "%"  { MOD }

  | "==" { EQUAL }
  | "!="  { NEQUAL }
  | "<"  { LOWER }
  | "<="  { LEQUAL }
  | ">"  { GREATER }
  | ">="  { GEQUAL }
  | "&&"  { AND }
  | "||"  { OR }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
