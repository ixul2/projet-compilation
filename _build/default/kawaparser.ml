
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WHILE
    | VOID
    | TRUE
    | THIS
    | STAR
    | SEMI
    | RPAR
    | RETURN
    | PRINT
    | PLUS
    | OR
    | NOT
    | NEW
    | NEQUAL
    | N of (
# 8 "kawaparser.mly"
       (int)
# 28 "kawaparser.ml"
  )
    | MOD
    | MINUS
    | MAIN
    | LPAR
    | LOWER
    | LEQUAL
    | INT
    | IF
    | IDENT of (
# 9 "kawaparser.mly"
       (string)
# 41 "kawaparser.ml"
  )
    | GREATER
    | GEQUAL
    | FALSE
    | EXTENDS
    | EQUAL
    | EOF
    | END
    | ELSE
    | DOT
    | DIV
    | COMMA
    | CLASS
    | BOOL
    | BEGIN
    | ASSIGN
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState136
  | MenhirState131
  | MenhirState126
  | MenhirState120
  | MenhirState110
  | MenhirState107
  | MenhirState101
  | MenhirState99
  | MenhirState96
  | MenhirState91
  | MenhirState87
  | MenhirState86
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState55
  | MenhirState53
  | MenhirState48
  | MenhirState44
  | MenhirState39
  | MenhirState38
  | MenhirState36
  | MenhirState33
  | MenhirState30
  | MenhirState28
  | MenhirState23
  | MenhirState21
  | MenhirState18
  | MenhirState12
  | MenhirState5
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa


# 119 "kawaparser.ml"

let rec _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 93 "kawaparser.mly"
        ( Get(m) )
# 127 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | FALSE ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | IDENT _v ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                    | IF ->
                        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | LPAR ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | MINUS ->
                        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | N _v ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                    | NEW ->
                        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | NOT ->
                        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | PRINT ->
                        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | RETURN ->
                        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | THIS ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | TRUE ->
                        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | WHILE ->
                        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | END ->
                        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))), _, (seq1 : (Kawa.seq))), _, (seq2 : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 82 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 218 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 234 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))), _, (seq : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 83 "kawaparser.mly"
                                                   ( While(e, seq) )
# 250 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (seq : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.seq) = 
# 34 "kawaparser.mly"
                                 ( seq )
# 272 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (glb : ((string * Kawa.typ) list))), _, (cls : (Kawa.class_def list))), (main_fun : (Kawa.seq))) = _menhir_stack in
                let _v : (Kawa.program) = 
# 30 "kawaparser.mly"
                                                           ( {classes=cls; globals=glb; main=main_fun} )
# 286 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (Kawa.program)) = _v in
                Obj.magic _1
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (id : (
# 9 "kawaparser.mly"
       (string)
# 323 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 328 "kawaparser.ml"
             in
            
# 99 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 333 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 354 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 359 "kawaparser.ml"
             in
            
# 98 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 364 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mem : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState101 | MenhirState96 | MenhirState91 | MenhirState87 | MenhirState30 | MenhirState33 | MenhirState36 | MenhirState38 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState48 | MenhirState44 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState131 | MenhirState120 | MenhirState28 | MenhirState86 | MenhirState110 | MenhirState107 | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | LPAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | N _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | NEW ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NOT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | THIS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | TRUE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | AND | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | SEMI | STAR ->
            _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_instr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState131 | MenhirState86 | MenhirState99 | MenhirState110 | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | IF ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LPAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | N _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | NEW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PRINT ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | RETURN ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | THIS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TRUE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | WHILE ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | MenhirState28 | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (Kawa.instr))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl) = 
# 62 "kawaparser.mly"
          ( Instr(i) )
# 474 "kawaparser.ml"
         in
        _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState36 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 490 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 501 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LPAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | N _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | NEW ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NOT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | THIS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | TRUE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 9 "kawaparser.mly"
       (string)
# 860 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 104 "kawaparser.mly"
                       ( Field(e, var) )
# 865 "kawaparser.ml"
             in
            _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_goto_list_class_attr_meth_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_attr_metho list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (cls_name : (
# 9 "kawaparser.mly"
       (string)
# 953 "kawaparser.ml"
            ))), (parent : (string option))), _, (attrs_methods : (Kawa.class_attr_metho list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 38 "kawaparser.mly"
                                                                                                    ( let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in
                                                                       let attrs = List.filter_map (fun m_v -> match m_v with Attr (id, t) -> Some (id, t) | _ -> None) attrs_methods in
                                                                       {class_name=cls_name; attributes=attrs; methods=methods; parent=parent} 
                                                                     )
# 961 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | MAIN ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_attr_metho))), _, (xs : (Kawa.class_attr_metho list))) = _menhir_stack in
        let _v : (Kawa.class_attr_metho list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 989 "kawaparser.ml"
         in
        _menhir_goto_list_class_attr_meth_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1000 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_instr_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (var : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))), _, (instr_var_decls : (Kawa.instr_var_decl list))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1021 "kawaparser.ml"
             in
            
# 53 "kawaparser.mly"
                                                                                                                           ( let loc = List.filter_map (fun v_i -> match v_i with Var_decl (v, t) -> Some(v, t) | _ -> None) instr_var_decls in
                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                             let id, ret = var in 
                                                                                                             {method_name=id; code=code; params=params; locals=loc; return=ret}
                                                                                                           )
# 1030 "kawaparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (m : (Kawa.method_def)) = _v in
            let _v : (Kawa.class_attr_metho) = 
# 49 "kawaparser.mly"
               ( Method(m) )
# 1038 "kawaparser.ml"
             in
            _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr_var_decl))), _, (xs : (Kawa.instr_var_decl list))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1054 "kawaparser.ml"
         in
        _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1065 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 1072 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce52 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "kawaparser.mly"
       (string)
# 1082 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (var : (
# 9 "kawaparser.mly"
       (string)
# 1088 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.mem_access) = 
# 103 "kawaparser.mly"
            ( Var(var) )
# 1093 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 96 "kawaparser.mly"
                   ( e )
# 1140 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 115 "kawaparser.mly"
       ( Mul )
# 1164 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1169 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 | MenhirState75 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LPAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | N _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | NEW ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | THIS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | TRUE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1244 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 113 "kawaparser.mly"
       ( Add )
# 1272 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1277 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 117 "kawaparser.mly"
      ( Rem )
# 1299 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1304 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 116 "kawaparser.mly"
      ( Div )
# 1326 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1331 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 125 "kawaparser.mly"
     ( Or )
# 1375 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1380 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 119 "kawaparser.mly"
         ( Neq )
# 1412 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1417 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 114 "kawaparser.mly"
        ( Sub )
# 1445 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1450 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 120 "kawaparser.mly"
        ( Lt )
# 1482 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1487 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 121 "kawaparser.mly"
         ( Le )
# 1519 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1524 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 122 "kawaparser.mly"
          ( Gt )
# 1556 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1561 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 123 "kawaparser.mly"
         ( Ge )
# 1593 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1598 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 118 "kawaparser.mly"
        ( Eq )
# 1630 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1635 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 124 "kawaparser.mly"
      ( And )
# 1679 "kawaparser.ml"
             in
            
# 95 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1684 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 108 "kawaparser.mly"
        ( Opp )
# 1706 "kawaparser.ml"
             in
            
# 94 "kawaparser.mly"
               ( Unop(u, e) )
# 1711 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 109 "kawaparser.mly"
      ( Not )
# 1733 "kawaparser.ml"
             in
            
# 94 "kawaparser.mly"
               ( Unop(u, e) )
# 1738 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | IDENT _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | IF ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | MINUS ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | N _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
                | NEW ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | NOT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | PRINT ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | RETURN ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | THIS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | TRUE ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | WHILE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | END ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 84 "kawaparser.mly"
                     ( Return(e) )
# 1873 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
                let _v : (Kawa.instr) = 
# 80 "kawaparser.mly"
                              ( Print(e) )
# 1928 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | IDENT _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | IF ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | MINUS ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | N _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | NEW ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | NOT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | PRINT ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | RETURN ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | THIS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | TRUE ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | WHILE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | END ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 81 "kawaparser.mly"
                             ( Set(mem, e) )
# 2071 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState131 | MenhirState28 | MenhirState120 | MenhirState86 | MenhirState99 | MenhirState107 | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 85 "kawaparser.mly"
              ( Expr(e) )
# 2121 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_attr_metho list) = 
# 211 "<standard.mly>"
    ( [] )
# 2140 "kawaparser.ml"
     in
    _menhir_goto_list_class_attr_meth_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_class_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MAIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | IDENT _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                | IF ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | LPAR ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | MINUS ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | N _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
                | NEW ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | NOT ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | PRINT ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | RETURN ->
                    _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | THIS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | TRUE ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | WHILE ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | END ->
                    _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState131
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2213 "kawaparser.ml"
         in
        _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_class_extension_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | END ->
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.instr_var_decl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2257 "kawaparser.ml"
     in
    _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | LPAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | N _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | NEW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | THIS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TRUE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 90 "kawaparser.mly"
       ( Bool(true) )
# 2308 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 92 "kawaparser.mly"
       ( This )
# 2319 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LPAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | N _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | NEW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | THIS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | TRUE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | IDENT _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | LPAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | N _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | NEW ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | NOT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | THIS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TRUE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | AND | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 2467 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 97 "kawaparser.mly"
               ( New(id) )
# 2472 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (int)
# 2491 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "kawaparser.mly"
       (int)
# 2499 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 89 "kawaparser.mly"
      ( Int(n) )
# 2504 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENT _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | IDENT _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | LPAR ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | N _v ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
        | NEW ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | THIS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | TRUE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 2609 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _ ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
    | AND | ASSIGN | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | SEMI | STAR ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 91 "kawaparser.mly"
        ( Bool(false) )
# 2634 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instr_var_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | FALSE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | IDENT _v ->
        _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | IF ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | LPAR ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | MINUS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | N _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | NEW ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | NOT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | PRINT ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | RETURN ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | THIS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | TRUE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | WHILE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | END ->
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState120
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_goto_class_attr_meth : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_attr_metho) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | END ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2711 "kawaparser.ml"
     in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (parent : (
# 9 "kawaparser.mly"
       (string)
# 2739 "kawaparser.ml"
                )) = _v in
                let _v : (string) = 
# 45 "kawaparser.mly"
                       ( parent )
# 2744 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2752 "kawaparser.ml"
                 in
                _menhir_goto_option_class_extension_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 2765 "kawaparser.ml"
             in
            _menhir_goto_option_class_extension_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2797 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 2807 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | FALSE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | IDENT _v ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IF ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LPAR ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | MINUS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | N _v ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | NEW ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NOT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | PRINT ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | RETURN ->
                _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | THIS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | TRUE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | WHILE ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | END ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail * _menhir_state * (string * Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (var : (string * Kawa.typ))) = _menhir_stack in
    let _v : (string * Kawa.typ) = 
# 65 "kawaparser.mly"
                          ( var )
# 2889 "kawaparser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | CLASS | MAIN ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | MenhirState126 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v_t : (string * Kawa.typ))) = _menhir_stack in
        let _v : (Kawa.class_attr_metho) = 
# 50 "kawaparser.mly"
               ( let v, t = v_t in Attr(v, t) )
# 2919 "kawaparser.ml"
         in
        _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v
    | MenhirState120 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (v_t : (string * Kawa.typ))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl) = 
# 61 "kawaparser.mly"
               ( let v, t = v_t in Var_decl(v, t) )
# 2929 "kawaparser.ml"
         in
        _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2946 "kawaparser.ml"
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLASS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MAIN ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        _menhir_fail ()

and _menhir_reduce65 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "kawaparser.mly"
       (string)
# 2968 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (cls_name : (
# 9 "kawaparser.mly"
       (string)
# 2974 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.typ) = 
# 76 "kawaparser.mly"
                 ( TClass(cls_name) )
# 2979 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (id : (
# 9 "kawaparser.mly"
       (string)
# 2997 "kawaparser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
        let _v : (string * Kawa.typ) = 
# 69 "kawaparser.mly"
                     ( (id, typ) )
# 3003 "kawaparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState120 | MenhirState28 | MenhirState0 | MenhirState5 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState126 | MenhirState18 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState21 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3044 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
            | SEMI ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState23 | MenhirState21 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3087 "kawaparser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 3268 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 75 "kawaparser.mly"
       ( TVoid )
# 3279 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 74 "kawaparser.mly"
      ( TInt )
# 3290 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 3297 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 73 "kawaparser.mly"
       ( TBool )
# 3311 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CLASS | MAIN ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3357 "kawaparser.ml"
