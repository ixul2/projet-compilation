
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
    | R_BRACKET
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
# 30 "kawaparser.ml"
  )
    | MOD
    | MINUS
    | MAIN
    | L_BRACKET
    | LPAR
    | LOWER
    | LEQUAL
    | INT
    | IF
    | IDENT of (
# 8 "kawaparser.mly"
       (string)
# 44 "kawaparser.ml"
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
  | MenhirState156
  | MenhirState151
  | MenhirState145
  | MenhirState139
  | MenhirState129
  | MenhirState126
  | MenhirState120
  | MenhirState118
  | MenhirState115
  | MenhirState110
  | MenhirState106
  | MenhirState105
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState61
  | MenhirState57
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState45
  | MenhirState42
  | MenhirState40
  | MenhirState34
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState24
  | MenhirState23
  | MenhirState17
  | MenhirState12
  | MenhirState8
  | MenhirState5
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa

# 131 "kawaparser.ml"

let rec _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 106 "kawaparser.mly"
        ( Get(m) )
# 139 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState118 ->
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
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | IDENT _v ->
                        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
                    | IF ->
                        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | LPAR ->
                        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | L_BRACKET ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | MINUS ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | N _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
                    | NEW ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | NOT ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | PRINT ->
                        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | RETURN ->
                        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | THIS ->
                        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | TRUE ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | WHILE ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | END ->
                        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState126
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
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
    | MenhirState126 ->
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
# 95 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 232 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 248 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState105 ->
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
# 96 "kawaparser.mly"
                                                   ( While(e, seq) )
# 264 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
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
# 286 "kawaparser.ml"
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
# 300 "kawaparser.ml"
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
    | MenhirState23 ->
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
# 337 "kawaparser.ml"
            ))), (parent : (string option))), _, (attrs_methods : (Kawa.class_attr_metho list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 37 "kawaparser.mly"
                                                                                                    ( (*here we get a list of both methods and attributes and we split them back. This choice means there's no constraint as far as the order : an attribute can be after a method*)
                                                                                                      let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in 
                                                                                                      let attrs = List.filter_map (fun m_v -> match m_v with Attr (attr) -> Some attr | _ -> None) attrs_methods in
                                                                                                      {class_name=cls_name; attributes=List.concat attrs; methods=methods; parent=parent} 
                                                                                                     )
# 346 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | MAIN ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState156
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_attr_metho))), _, (xs : (Kawa.class_attr_metho list))) = _menhir_stack in
        let _v : (Kawa.class_attr_metho list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 374 "kawaparser.ml"
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
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | FINAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | END ->
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState61 ->
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
# 420 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 425 "kawaparser.ml"
             in
            
# 112 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 430 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | R_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let exprs = 
# 232 "<standard.mly>"
    ( xs )
# 452 "kawaparser.ml"
             in
            
# 113 "kawaparser.mly"
                                                        ( Array(exprs) )
# 457 "kawaparser.ml"
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
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 8 "kawaparser.mly"
       (string)
# 478 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 483 "kawaparser.ml"
             in
            
# 111 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 488 "kawaparser.ml"
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
    | MenhirState120 | MenhirState115 | MenhirState110 | MenhirState106 | MenhirState42 | MenhirState45 | MenhirState48 | MenhirState50 | MenhirState51 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState61 | MenhirState57 | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState151 | MenhirState139 | MenhirState40 | MenhirState105 | MenhirState129 | MenhirState126 | MenhirState118 ->
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | L_BRACKET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | MINUS ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | N _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | NEW ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | NOT ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | THIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | TRUE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState120
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
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
    | MenhirState151 | MenhirState105 | MenhirState118 | MenhirState129 | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | IDENT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | IF ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | L_BRACKET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | MINUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | N _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | NEW ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | NOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PRINT ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | RETURN ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | THIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | TRUE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | WHILE ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | END ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState40 | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (Kawa.instr))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl) = 
# 72 "kawaparser.mly"
          ( Instr(i) )
# 602 "kawaparser.ml"
         in
        _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState48 | MenhirState51 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 618 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 629 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | L_BRACKET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | MINUS ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | N _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | NEW ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | NOT ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | THIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | TRUE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | RPAR ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 8 "kawaparser.mly"
       (string)
# 1072 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 119 "kawaparser.mly"
                       ( Field(e, var) )
# 1077 "kawaparser.ml"
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

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1158 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_attr_metho list) = 
# 211 "<standard.mly>"
    ( [] )
# 1167 "kawaparser.ml"
     in
    _menhir_goto_list_class_attr_meth_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_goto_attr_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.attribute_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (attr : (Kawa.attribute_def list)) = _v in
    let _v : (Kawa.class_attr_metho) = 
# 50 "kawaparser.mly"
                 ( Attr(attr) )
# 1198 "kawaparser.ml"
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
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | IDENT _v ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | IF ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | PRINT ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | RETURN ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | WHILE ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | END ->
        _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_goto_list_instr_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState40 ->
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
# 1266 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (instr_var_decls : (Kawa.instr_var_decl list))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1271 "kawaparser.ml"
             in
            
# 58 "kawaparser.mly"
                                                                                                                           ( (*we use the same trick for variables and instructions in method_def as we did for attributes and methods in class_def*)
                                                                                                                             let loc = List.filter_map (fun v_i -> match v_i with Var_decl vars -> Some vars | _ -> None) instr_var_decls in
                                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                                             {method_name=id; code=code; params=params; locals=List.concat loc; return=ret}
                                                                                                                           )
# 1280 "kawaparser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (m : (Kawa.method_def)) = _v in
            let _v : (Kawa.class_attr_metho) = 
# 49 "kawaparser.mly"
               ( Method(m) )
# 1288 "kawaparser.ml"
             in
            _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr_var_decl))), _, (xs : (Kawa.instr_var_decl list))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1304 "kawaparser.ml"
         in
        _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1315 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 1322 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce57 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 1332 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (var : (
# 8 "kawaparser.mly"
       (string)
# 1338 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.mem_access) = 
# 118 "kawaparser.mly"
            ( Var(var) )
# 1343 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 109 "kawaparser.mly"
                   ( e )
# 1394 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 130 "kawaparser.mly"
       ( Mul )
# 1418 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1423 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 | MenhirState51 | MenhirState92 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | L_BRACKET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | MINUS ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | N _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | NEW ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NOT ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | THIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | TRUE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | RPAR | R_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1504 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 128 "kawaparser.mly"
       ( Add )
# 1532 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1537 "kawaparser.ml"
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
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 132 "kawaparser.mly"
      ( Rem )
# 1559 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1564 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 131 "kawaparser.mly"
      ( Div )
# 1586 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1591 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 140 "kawaparser.mly"
     ( Or )
# 1639 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1644 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 142 "kawaparser.mly"
                ( Neq_struct )
# 1676 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1681 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 129 "kawaparser.mly"
        ( Sub )
# 1709 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1714 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 134 "kawaparser.mly"
         ( Neq )
# 1746 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1751 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 135 "kawaparser.mly"
        ( Lt )
# 1783 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1788 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 136 "kawaparser.mly"
         ( Le )
# 1820 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1825 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 137 "kawaparser.mly"
          ( Gt )
# 1857 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1862 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 138 "kawaparser.mly"
         ( Ge )
# 1894 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1899 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 141 "kawaparser.mly"
               ( Eq_struct )
# 1931 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1936 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 133 "kawaparser.mly"
        ( Eq )
# 1968 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1973 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 139 "kawaparser.mly"
      ( And )
# 2021 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 2026 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 123 "kawaparser.mly"
        ( Opp )
# 2048 "kawaparser.ml"
             in
            
# 107 "kawaparser.mly"
               ( Unop(u, e) )
# 2053 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 124 "kawaparser.mly"
      ( Not )
# 2075 "kawaparser.ml"
             in
            
# 107 "kawaparser.mly"
               ( Unop(u, e) )
# 2080 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | IDENT _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
                | IF ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | LPAR ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | L_BRACKET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | MINUS ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | N _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
                | NEW ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | NOT ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | PRINT ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | RETURN ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | THIS ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | TRUE ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | WHILE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | END ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 97 "kawaparser.mly"
                     ( Return(e) )
# 2225 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
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
# 93 "kawaparser.mly"
                              ( Print(e) )
# 2284 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | IDENT _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | IF ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | LPAR ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | L_BRACKET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | MINUS ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | N _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | NEW ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | NOT ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | PRINT ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | RETURN ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | THIS ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | TRUE ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | WHILE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | END ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 94 "kawaparser.mly"
                             ( Set(mem, e) )
# 2437 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 | MenhirState40 | MenhirState139 | MenhirState105 | MenhirState118 | MenhirState126 | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 98 "kawaparser.mly"
              ( Expr(e) )
# 2491 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState17 ->
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
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | IDENT _v ->
                    _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
                | IF ->
                    _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | LPAR ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | L_BRACKET ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | MINUS ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | N _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
                | NEW ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | NOT ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | PRINT ->
                    _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | RETURN ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | THIS ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | TRUE ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | WHILE ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | END ->
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
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
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2576 "kawaparser.ml"
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | FINAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | END ->
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_COMMA_IDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 8 "kawaparser.mly"
       (string)
# 2627 "kawaparser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2632 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ids : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 80 "kawaparser.mly"
                                                       ( List.map (fun id -> (id, typ)) ids )
# 2643 "kawaparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState139 | MenhirState40 | MenhirState0 | MenhirState5 ->
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
# 76 "kawaparser.mly"
                            ( vars )
# 2660 "kawaparser.ml"
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
                        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
                | MenhirState139 | MenhirState40 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                    let _v : (Kawa.instr_var_decl) = 
# 71 "kawaparser.mly"
                ( Var_decl(vars) )
# 2690 "kawaparser.ml"
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
        | MenhirState24 ->
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
# 54 "kawaparser.mly"
                                  (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=true}) vars)
# 2714 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState145 | MenhirState23 ->
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
# 53 "kawaparser.mly"
                            (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=false}) vars)
# 2736 "kawaparser.ml"
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

and _menhir_reduce48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.instr_var_decl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2755 "kawaparser.ml"
     in
    _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | IDENT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | L_BRACKET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | MINUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | N _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | NEW ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | NOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | THIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | TRUE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 103 "kawaparser.mly"
       ( Bool(true) )
# 2808 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 105 "kawaparser.mly"
       ( This )
# 2819 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | IDENT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | L_BRACKET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MINUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | N _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
        | NEW ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | THIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | TRUE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState110
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

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | IDENT _v ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | L_BRACKET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | MINUS ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | N _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | NEW ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | NOT ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | THIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | TRUE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | RPAR ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | AND | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 8 "kawaparser.mly"
       (string)
# 2975 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 110 "kawaparser.mly"
               ( New(id) )
# 2980 "kawaparser.ml"
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

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "kawaparser.mly"
       (int)
# 2999 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "kawaparser.mly"
       (int)
# 3007 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 102 "kawaparser.mly"
      ( Int(n) )
# 3012 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | R_BRACKET ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENT _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAR ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | L_BRACKET ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | N _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | NEW ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | THIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | IDENT _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | LPAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | L_BRACKET ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MINUS ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | N _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | NEW ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NOT ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | THIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | TRUE ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3156 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _ | L_BRACKET ->
        _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack)
    | AND | ASSIGN | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | SEMI | STAR ->
        _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 104 "kawaparser.mly"
        ( Bool(false) )
# 3181 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 3190 "kawaparser.ml"
     in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 3218 "kawaparser.ml"
                )) = _v in
                let _v : (string) = 
# 45 "kawaparser.mly"
                       ( parent )
# 3223 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 3231 "kawaparser.ml"
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
# 3244 "kawaparser.ml"
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
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3276 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 3286 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce63 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3295 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 8 "kawaparser.mly"
       (string)
# 3301 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3306 "kawaparser.ml"
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FALSE ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | IDENT _v ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | IF ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LPAR ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | L_BRACKET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | MINUS ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | N _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | NEW ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NOT ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | PRINT ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | RETURN ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | THIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TRUE ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | VOID ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | WHILE ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | END ->
                _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
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

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3383 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run9 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.typ) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | R_BRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t : (Kawa.typ))), _) = _menhir_stack in
        let _v : (Kawa.typ) = 
# 89 "kawaparser.mly"
                              ( TArray(t) )
# 3410 "kawaparser.ml"
         in
        _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3423 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
    | SEMI ->
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack)
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
# 3452 "kawaparser.ml"
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLASS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | MAIN ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3474 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (cls_name : (
# 8 "kawaparser.mly"
       (string)
# 3480 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.typ) = 
# 88 "kawaparser.mly"
                 ( TClass(cls_name) )
# 3485 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 | MenhirState40 | MenhirState24 | MenhirState0 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | MenhirState145 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState29 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
            | LPAR ->
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
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState31 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3539 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
            | SEMI ->
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | MenhirState31 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState34 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 8 "kawaparser.mly"
       (string)
# 3573 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 66 "kawaparser.mly"
                     ( (id, typ) )
# 3579 "kawaparser.ml"
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3609 "kawaparser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
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
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
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

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list list) = 
# 211 "<standard.mly>"
    ( [] )
# 3822 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 87 "kawaparser.mly"
       ( TVoid )
# 3833 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 86 "kawaparser.mly"
      ( TInt )
# 3844 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3851 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 85 "kawaparser.mly"
       ( TBool )
# 3865 "kawaparser.ml"
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
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3911 "kawaparser.ml"
