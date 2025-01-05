# 1 "kawalexer.mll"
 

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
        

<<<<<<< HEAD
# 35 "kawalexer.ml"
=======
# 39 "kawalexer.ml"
>>>>>>> 541246b (idk anymore)
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\226\255\227\255\002\000\001\000\002\000\003\000\004\000\
    \237\255\239\255\240\255\241\255\242\255\243\255\244\255\245\255\
    \246\255\247\255\248\255\005\000\079\000\019\000\035\000\002\000\
    \255\255\252\255\002\000\253\255\235\255\234\255\232\255\230\255\
    \229\255\228\255\016\000\253\255\254\255\031\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\028\000\028\000\024\000\022\000\028\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\006\000\005\000\004\000\017\000\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\036\000\000\000\000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\024\000\023\000\027\000\023\000\000\000\023\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \023\000\007\000\023\000\000\000\000\000\008\000\004\000\032\000\
    \015\000\014\000\009\000\011\000\016\000\010\000\017\000\022\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\037\000\018\000\006\000\019\000\005\000\031\000\
    \030\000\029\000\028\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\025\000\038\000\000\000\
    \000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
    \000\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\013\000\003\000\012\000\033\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\020\000\000\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\023\000\026\000\000\000\255\255\023\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\023\000\255\255\255\255\000\000\000\000\004\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\034\000\000\000\000\000\000\000\000\000\005\000\
    \006\000\007\000\019\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\022\000\037\000\255\255\
    \255\255\255\255\022\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\003\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\020\000\255\255\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\026\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \034\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
<<<<<<< HEAD
# 40 "kawalexer.mll"
                      ( new_line lexbuf; token lexbuf )
# 164 "kawalexer.ml"

  | 1 ->
# 41 "kawalexer.mll"
                      ( token lexbuf )
# 169 "kawalexer.ml"

  | 2 ->
# 43 "kawalexer.mll"
                         ( new_line lexbuf; token lexbuf )
# 174 "kawalexer.ml"

  | 3 ->
# 44 "kawalexer.mll"
                         ( comment lexbuf; token lexbuf )
# 179 "kawalexer.ml"

  | 4 ->
let
# 46 "kawalexer.mll"
              n
# 185 "kawalexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 46 "kawalexer.mll"
                 ( N(int_of_string n) )
# 189 "kawalexer.ml"

  | 5 ->
let
# 47 "kawalexer.mll"
             id
# 195 "kawalexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 47 "kawalexer.mll"
                 ( match (List.find_opt (fun x -> x=id) ["var"; "attribute"; "method"]) with
                     None -> keyword_or_ident id
                   | _ -> token lexbuf )
# 201 "kawalexer.ml"

  | 6 ->
# 52 "kawalexer.mll"
         ( ASSIGN )
# 206 "kawalexer.ml"

  | 7 ->
# 53 "kawalexer.mll"
         ( SEMI )
# 211 "kawalexer.ml"

  | 8 ->
# 54 "kawalexer.mll"
         ( DOT )
# 216 "kawalexer.ml"

  | 9 ->
# 55 "kawalexer.mll"
         ( COMMA )
# 221 "kawalexer.ml"

  | 10 ->
# 56 "kawalexer.mll"
         ( LPAR )
# 226 "kawalexer.ml"

  | 11 ->
# 57 "kawalexer.mll"
         ( RPAR )
# 231 "kawalexer.ml"

  | 12 ->
# 58 "kawalexer.mll"
         ( BEGIN )
# 236 "kawalexer.ml"

  | 13 ->
# 59 "kawalexer.mll"
         ( END )
# 241 "kawalexer.ml"

  | 14 ->
# 61 "kawalexer.mll"
         ( PLUS )
# 246 "kawalexer.ml"

  | 15 ->
# 62 "kawalexer.mll"
         ( MINUS )
# 251 "kawalexer.ml"

  | 16 ->
# 63 "kawalexer.mll"
         ( STAR )
# 256 "kawalexer.ml"

  | 17 ->
# 64 "kawalexer.mll"
         ( DIV )
# 261 "kawalexer.ml"

  | 18 ->
# 65 "kawalexer.mll"
         ( MOD )
# 266 "kawalexer.ml"

  | 19 ->
# 66 "kawalexer.mll"
         ( MOD )
# 271 "kawalexer.ml"

  | 20 ->
# 68 "kawalexer.mll"
         ( EQUAL )
# 276 "kawalexer.ml"

  | 21 ->
# 69 "kawalexer.mll"
          ( NEQUAL )
# 281 "kawalexer.ml"

  | 22 ->
# 70 "kawalexer.mll"
         ( LOWER )
# 286 "kawalexer.ml"

  | 23 ->
# 71 "kawalexer.mll"
          ( LEQUAL )
# 291 "kawalexer.ml"

  | 24 ->
# 72 "kawalexer.mll"
         ( GREATER )
# 296 "kawalexer.ml"

  | 25 ->
# 73 "kawalexer.mll"
          ( GEQUAL )
# 301 "kawalexer.ml"

  | 26 ->
# 74 "kawalexer.mll"
          ( AND )
# 306 "kawalexer.ml"

  | 27 ->
# 75 "kawalexer.mll"
          ( OR )
# 311 "kawalexer.ml"

  | 28 ->
# 77 "kawalexer.mll"
         ( raise (Error ("unknown character : " ^ lexeme lexbuf)) )
# 316 "kawalexer.ml"

  | 29 ->
# 78 "kawalexer.mll"
         ( EOF )
# 321 "kawalexer.ml"
=======
# 44 "kawalexer.mll"
                      ( new_line lexbuf; token lexbuf )
# 168 "kawalexer.ml"

  | 1 ->
# 45 "kawalexer.mll"
                      ( token lexbuf )
# 173 "kawalexer.ml"

  | 2 ->
# 47 "kawalexer.mll"
                         ( new_line lexbuf; token lexbuf )
# 178 "kawalexer.ml"

  | 3 ->
# 48 "kawalexer.mll"
                         ( comment lexbuf; token lexbuf )
# 183 "kawalexer.ml"

  | 4 ->
let
# 50 "kawalexer.mll"
              n
# 189 "kawalexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 50 "kawalexer.mll"
                 ( N(int_of_string n) )
# 193 "kawalexer.ml"

  | 5 ->
let
# 51 "kawalexer.mll"
             id
# 199 "kawalexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 51 "kawalexer.mll"
                 ( keyword_or_ident id )
# 203 "kawalexer.ml"

  | 6 ->
# 54 "kawalexer.mll"
         ( ASSIGN )
# 208 "kawalexer.ml"

  | 7 ->
# 55 "kawalexer.mll"
         ( SEMI )
# 213 "kawalexer.ml"

  | 8 ->
# 56 "kawalexer.mll"
         ( DOT )
# 218 "kawalexer.ml"

  | 9 ->
# 57 "kawalexer.mll"
         ( COMMA )
# 223 "kawalexer.ml"

  | 10 ->
# 58 "kawalexer.mll"
         ( LPAR )
# 228 "kawalexer.ml"

  | 11 ->
# 59 "kawalexer.mll"
         ( RPAR )
# 233 "kawalexer.ml"

  | 12 ->
# 60 "kawalexer.mll"
         ( BEGIN )
# 238 "kawalexer.ml"

  | 13 ->
# 61 "kawalexer.mll"
         ( END )
# 243 "kawalexer.ml"

  | 14 ->
# 63 "kawalexer.mll"
         ( PLUS )
# 248 "kawalexer.ml"

  | 15 ->
# 64 "kawalexer.mll"
         ( MINUS )
# 253 "kawalexer.ml"

  | 16 ->
# 65 "kawalexer.mll"
         ( STAR )
# 258 "kawalexer.ml"

  | 17 ->
# 66 "kawalexer.mll"
         ( DIV )
# 263 "kawalexer.ml"

  | 18 ->
# 67 "kawalexer.mll"
         ( MOD )
# 268 "kawalexer.ml"

  | 19 ->
# 68 "kawalexer.mll"
         ( MOD )
# 273 "kawalexer.ml"

  | 20 ->
# 70 "kawalexer.mll"
         ( EQUAL )
# 278 "kawalexer.ml"

  | 21 ->
# 71 "kawalexer.mll"
          ( NEQUAL )
# 283 "kawalexer.ml"

  | 22 ->
# 72 "kawalexer.mll"
         ( LOWER )
# 288 "kawalexer.ml"

  | 23 ->
# 73 "kawalexer.mll"
          ( LEQUAL )
# 293 "kawalexer.ml"

  | 24 ->
# 74 "kawalexer.mll"
         ( GREATER )
# 298 "kawalexer.ml"

  | 25 ->
# 75 "kawalexer.mll"
          ( GEQUAL )
# 303 "kawalexer.ml"

  | 26 ->
# 76 "kawalexer.mll"
          ( AND )
# 308 "kawalexer.ml"

  | 27 ->
# 77 "kawalexer.mll"
          ( OR )
# 313 "kawalexer.ml"

  | 28 ->
# 79 "kawalexer.mll"
         ( raise (Error ("unknown character : " ^ lexeme lexbuf)) )
# 318 "kawalexer.ml"

  | 29 ->
# 80 "kawalexer.mll"
         ( EOF )
# 323 "kawalexer.ml"
>>>>>>> 541246b (idk anymore)

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 34
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
<<<<<<< HEAD
# 81 "kawalexer.mll"
         ( () )
# 333 "kawalexer.ml"

  | 1 ->
# 82 "kawalexer.mll"
         ( comment lexbuf )
# 338 "kawalexer.ml"

  | 2 ->
# 83 "kawalexer.mll"
         ( raise (Error "unterminated comment") )
# 343 "kawalexer.ml"
=======
# 83 "kawalexer.mll"
         ( () )
# 335 "kawalexer.ml"

  | 1 ->
# 84 "kawalexer.mll"
         ( comment lexbuf )
# 340 "kawalexer.ml"

  | 2 ->
# 85 "kawalexer.mll"
         ( raise (Error "unterminated comment") )
# 345 "kawalexer.ml"
>>>>>>> 541246b (idk anymore)

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

