
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
    | NEQUAL_STRUCT
    | NEQUAL
    | N of (
# 7 "kawaparser.mly"
       (int)
# 29 "kawaparser.ml"
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
# 8 "kawaparser.mly"
       (string)
# 42 "kawaparser.ml"
  )
    | GREATER
    | GEQUAL
    | FINAL
    | FALSE
    | EXTENDS
    | EQUAL_STRUCT
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
  | MenhirState151
  | MenhirState146
  | MenhirState140
  | MenhirState134
  | MenhirState124
  | MenhirState121
  | MenhirState115
  | MenhirState113
  | MenhirState110
  | MenhirState105
  | MenhirState101
  | MenhirState100
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState72
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState49
  | MenhirState48
  | MenhirState46
  | MenhirState43
  | MenhirState40
  | MenhirState38
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState22
  | MenhirState21
  | MenhirState15
  | MenhirState10
  | MenhirState8
  | MenhirState5
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa

# 127 "kawaparser.ml"

let rec _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 102 "kawaparser.mly"
        ( Get(m) )
# 135 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 ->
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
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | IDENT _v ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                    | IF ->
                        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | LPAR ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | MINUS ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | N _v ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
                    | NEW ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | NOT ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | PRINT ->
                        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | RETURN ->
                        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | THIS ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | TRUE ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | WHILE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | END ->
                        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
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
    | MenhirState121 ->
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
# 91 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 226 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 242 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState100 ->
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
# 92 "kawaparser.mly"
                                                   ( While(e, seq) )
# 258 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 ->
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
# 33 "kawaparser.mly"
                                 ( seq )
# 280 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s, (glb : ((string * Kawa.typ) list list))), _, (cls : (Kawa.class_def list))), (main_fun : (Kawa.seq))) = _menhir_stack in
                let _v : (Kawa.program) = 
# 29 "kawaparser.mly"
                                                           ( {classes=cls; globals= List.concat glb; main=main_fun} )
# 294 "kawaparser.ml"
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

and _menhir_goto_list_class_attr_meth_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_attr_metho list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (cls_name : (
# 8 "kawaparser.mly"
       (string)
# 331 "kawaparser.ml"
            ))), (parent : (string option))), _, (attrs_methods : (Kawa.class_attr_metho list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 37 "kawaparser.mly"
                                                                                                    ( let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in
                                                                       let attrs = List.filter_map (fun m_v -> match m_v with Attr (attr) -> Some attr | _ -> None) attrs_methods in
                                                                       {class_name=cls_name; attributes=List.concat attrs; methods=methods; parent=parent} 
                                                                     )
# 339 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | MAIN ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_attr_metho))), _, (xs : (Kawa.class_attr_metho list))) = _menhir_stack in
        let _v : (Kawa.class_attr_metho list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 367 "kawaparser.ml"
         in
        _menhir_goto_list_class_attr_meth_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_class_attr_meth : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_attr_metho) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | FINAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | END ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (id : (
# 8 "kawaparser.mly"
       (string)
# 413 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 418 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 423 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 8 "kawaparser.mly"
       (string)
# 444 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 449 "kawaparser.ml"
             in
            
# 107 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 454 "kawaparser.ml"
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
    | MenhirState115 | MenhirState110 | MenhirState105 | MenhirState101 | MenhirState40 | MenhirState43 | MenhirState46 | MenhirState48 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState72 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState146 | MenhirState134 | MenhirState38 | MenhirState100 | MenhirState124 | MenhirState121 | MenhirState113 ->
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | IDENT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | LPAR ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | MINUS ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | N _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | NEW ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | NOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | THIS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | TRUE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | AND | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | SEMI | STAR ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState146 | MenhirState100 | MenhirState113 | MenhirState124 | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | IDENT _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | IF ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LPAR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | N _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
        | NEW ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PRINT ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | RETURN ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | THIS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | TRUE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | WHILE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | END ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
    | MenhirState38 | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (Kawa.instr))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl) = 
# 65 "kawaparser.mly"
          ( Instr(i) )
# 564 "kawaparser.ml"
         in
        _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 580 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 591 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | IDENT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | LPAR ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | MINUS ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | N _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | NEW ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | NOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | THIS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | TRUE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | RPAR ->
                _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 8 "kawaparser.mly"
       (string)
# 1006 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 113 "kawaparser.mly"
                       ( Field(e, var) )
# 1011 "kawaparser.ml"
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

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1088 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_attr_metho list) = 
# 211 "<standard.mly>"
    ( [] )
# 1097 "kawaparser.ml"
     in
    _menhir_goto_list_class_attr_meth_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_goto_attr_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.attribute_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (attr : (Kawa.attribute_def list)) = _v in
    let _v : (Kawa.class_attr_metho) = 
# 49 "kawaparser.mly"
                 ( Attr(attr) )
# 1128 "kawaparser.ml"
     in
    _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instr_var_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | IDENT _v ->
        _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | IF ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | PRINT ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | RETURN ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | WHILE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | END ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_goto_list_instr_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (ret : (Kawa.typ))), _, (id : (
# 8 "kawaparser.mly"
       (string)
# 1194 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (instr_var_decls : (Kawa.instr_var_decl list))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1199 "kawaparser.ml"
             in
            
# 52 "kawaparser.mly"
                                                                                                                           ( let loc = List.filter_map (fun v_i -> match v_i with Var_decl vars -> Some vars | _ -> None) instr_var_decls in
                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                             {method_name=id; code=code; params=params; locals=List.concat loc; return=ret}
                                                                                                           )
# 1207 "kawaparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (m : (Kawa.method_def)) = _v in
            let _v : (Kawa.class_attr_metho) = 
# 48 "kawaparser.mly"
               ( Method(m) )
# 1215 "kawaparser.ml"
             in
            _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr_var_decl))), _, (xs : (Kawa.instr_var_decl list))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1231 "kawaparser.ml"
         in
        _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1242 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 1249 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 1259 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (var : (
# 8 "kawaparser.mly"
       (string)
# 1265 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.mem_access) = 
# 112 "kawaparser.mly"
            ( Var(var) )
# 1270 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 105 "kawaparser.mly"
                   ( e )
# 1321 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 124 "kawaparser.mly"
       ( Mul )
# 1349 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1354 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 136 "kawaparser.mly"
                ( Neq_struct )
# 1406 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1411 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 122 "kawaparser.mly"
       ( Add )
# 1443 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1448 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 126 "kawaparser.mly"
      ( Rem )
# 1474 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1479 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 135 "kawaparser.mly"
               ( Eq_struct )
# 1531 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1536 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 134 "kawaparser.mly"
     ( Or )
# 1584 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1589 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 128 "kawaparser.mly"
         ( Neq )
# 1625 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1630 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 123 "kawaparser.mly"
        ( Sub )
# 1662 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1667 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 | MenhirState89 | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IDENT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | LPAR ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | MINUS ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | N _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | NEW ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | NOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | THIS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | TRUE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1746 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 129 "kawaparser.mly"
        ( Lt )
# 1782 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1787 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 125 "kawaparser.mly"
      ( Div )
# 1813 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1818 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 130 "kawaparser.mly"
         ( Le )
# 1854 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1859 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 131 "kawaparser.mly"
          ( Gt )
# 1895 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1900 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 132 "kawaparser.mly"
         ( Ge )
# 1936 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1941 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 127 "kawaparser.mly"
        ( Eq )
# 1977 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1982 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 133 "kawaparser.mly"
      ( And )
# 2030 "kawaparser.ml"
             in
            
# 104 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 2035 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 117 "kawaparser.mly"
        ( Opp )
# 2061 "kawaparser.ml"
             in
            
# 103 "kawaparser.mly"
               ( Unop(u, e) )
# 2066 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 118 "kawaparser.mly"
      ( Not )
# 2092 "kawaparser.ml"
             in
            
# 103 "kawaparser.mly"
               ( Unop(u, e) )
# 2097 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | IDENT _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | IF ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | LPAR ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | MINUS ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | N _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
                | NEW ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | NOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | PRINT ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | RETURN ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | THIS ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | TRUE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | WHILE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 93 "kawaparser.mly"
                     ( Return(e) )
# 2240 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
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
# 89 "kawaparser.mly"
                              ( Print(e) )
# 2299 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | IDENT _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | IF ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | LPAR ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | MINUS ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | N _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
                | NEW ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | NOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | PRINT ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | RETURN ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | THIS ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | TRUE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | WHILE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 90 "kawaparser.mly"
                             ( Set(mem, e) )
# 2450 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState146 | MenhirState38 | MenhirState134 | MenhirState100 | MenhirState113 | MenhirState121 | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 94 "kawaparser.mly"
              ( Expr(e) )
# 2504 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_class_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState15 ->
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
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | IDENT _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                | IF ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | LPAR ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | MINUS ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | N _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
                | NEW ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | NOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | PRINT ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | RETURN ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | THIS ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | TRUE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | WHILE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState146
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146)
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
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2587 "kawaparser.ml"
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | FINAL ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_IDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 8 "kawaparser.mly"
       (string)
# 2638 "kawaparser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2643 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ids : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 78 "kawaparser.mly"
                                                       ( List.map (fun id -> (id, typ)) ids )
# 2654 "kawaparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState134 | MenhirState38 | MenhirState0 | MenhirState5 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 74 "kawaparser.mly"
                            ( vars )
# 2671 "kawaparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
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
                        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
                | MenhirState134 | MenhirState38 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                    let _v : (Kawa.instr_var_decl) = 
# 64 "kawaparser.mly"
                ( Var_decl(vars) )
# 2701 "kawaparser.ml"
                     in
                    _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState22 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let _v : (Kawa.attribute_def list) = 
# 70 "kawaparser.mly"
                                  (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=true}) vars)
# 2725 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState140 | MenhirState21 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let _v : (Kawa.attribute_def list) = 
# 69 "kawaparser.mly"
                            (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=false}) vars)
# 2747 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        _menhir_fail ()

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.instr_var_decl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2766 "kawaparser.ml"
     in
    _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | IDENT _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LPAR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | N _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | NEW ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | NOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | THIS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | TRUE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 99 "kawaparser.mly"
       ( Bool(true) )
# 2817 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 101 "kawaparser.mly"
       ( This )
# 2828 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | IDENT _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | LPAR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | N _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | NEW ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | NOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | THIS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | TRUE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IDENT _v ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | LPAR ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | MINUS ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | N _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | NEW ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | NOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | THIS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TRUE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | RPAR ->
                _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | AND | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 8 "kawaparser.mly"
       (string)
# 2976 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 106 "kawaparser.mly"
               ( New(id) )
# 2981 "kawaparser.ml"
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

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kawaparser.mly"
       (int)
# 3000 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "kawaparser.mly"
       (int)
# 3008 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 98 "kawaparser.mly"
      ( Int(n) )
# 3013 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAR ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MINUS ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | N _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | NEW ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | THIS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | IDENT _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | LPAR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | N _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | NEW ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | THIS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TRUE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3118 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _ ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
    | AND | ASSIGN | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | SEMI | STAR ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 100 "kawaparser.mly"
        ( Bool(false) )
# 3143 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 3152 "kawaparser.ml"
     in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 8 "kawaparser.mly"
       (string)
# 3180 "kawaparser.ml"
                )) = _v in
                let _v : (string) = 
# 44 "kawaparser.mly"
                       ( parent )
# 3185 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 3193 "kawaparser.ml"
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
# 3206 "kawaparser.ml"
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3238 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 3248 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce62 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3257 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 8 "kawaparser.mly"
       (string)
# 3263 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3268 "kawaparser.ml"
     in
    _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v

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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FALSE ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | IDENT _v ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | IF ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LPAR ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | MINUS ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | N _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NEW ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PRINT ->
                _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | RETURN ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | THIS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | WHILE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | END ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
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

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3343 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3359 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
    | SEMI ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : ((string * Kawa.typ) list))), _, (xs : ((string * Kawa.typ) list list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3388 "kawaparser.ml"
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLASS ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | MAIN ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3410 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (cls_name : (
# 8 "kawaparser.mly"
       (string)
# 3416 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.typ) = 
# 85 "kawaparser.mly"
                 ( TClass(cls_name) )
# 3421 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState134 | MenhirState38 | MenhirState22 | MenhirState0 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | MenhirState140 | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState27 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState29 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3473 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
            | SEMI ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState29 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 8 "kawaparser.mly"
       (string)
# 3504 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 59 "kawaparser.mly"
                     ( (id, typ) )
# 3510 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3540 "kawaparser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
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

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list list) = 
# 211 "<standard.mly>"
    ( [] )
# 3745 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 84 "kawaparser.mly"
       ( TVoid )
# 3756 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 83 "kawaparser.mly"
      ( TInt )
# 3767 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3774 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 82 "kawaparser.mly"
       ( TBool )
# 3788 "kawaparser.ml"
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
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3834 "kawaparser.ml"
