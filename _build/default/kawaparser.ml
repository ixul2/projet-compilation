
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
    | FINAL
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
<<<<<<< HEAD
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
=======
  | MenhirState152
  | MenhirState147
  | MenhirState142
  | MenhirState141
  | MenhirState139
  | MenhirState135
  | MenhirState127
  | MenhirState124
  | MenhirState118
  | MenhirState116
  | MenhirState113
  | MenhirState108
  | MenhirState104
  | MenhirState103
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
>>>>>>> 541246b (idk anymore)
  | MenhirState65
  | MenhirState61
<<<<<<< HEAD
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
=======
  | MenhirState56
  | MenhirState55
  | MenhirState53
  | MenhirState50
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState37
  | MenhirState35
  | MenhirState32
  | MenhirState31
  | MenhirState27
  | MenhirState23
  | MenhirState21
  | MenhirState16
  | MenhirState13
  | MenhirState8
  | MenhirState6
  | MenhirState1
>>>>>>> 541246b (idk anymore)
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa


<<<<<<< HEAD
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

=======
# 133 "kawaparser.ml"

let rec _menhir_goto_class_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | MAIN ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState152
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152

and _menhir_goto_list_method_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.method_def))), _, (xs : (Kawa.method_def list))) = _menhir_stack in
        let _v : (Kawa.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 162 "kawaparser.ml"
         in
        _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (cls_name : (
# 9 "kawaparser.mly"
       (string)
# 177 "kawaparser.ml"
            ))), (parent : (
# 9 "kawaparser.mly"
       (string)
# 181 "kawaparser.ml"
            ))), _, (attrs : (Kawa.attribute_def list))), _, (methods : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 35 "kawaparser.mly"
                                                                                                     ( {class_name=cls_name; attributes=attrs; methods=methods; parent=Some parent} )
# 186 "kawaparser.ml"
             in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
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
# 207 "kawaparser.ml"
            ))), _, (attrs : (Kawa.attribute_def list))), _, (methods : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 34 "kawaparser.mly"
                                                                                ( {class_name=cls_name; attributes=attrs; methods=methods; parent=None} )
# 212 "kawaparser.ml"
             in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 229 "kawaparser.ml"
     in
    _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState65 ->
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
# 268 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 273 "kawaparser.ml"
             in
            
# 68 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 278 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 299 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 304 "kawaparser.ml"
             in
            
# 67 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 309 "kawaparser.ml"
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

and _menhir_reduce9 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 62 "kawaparser.mly"
        ( Get(m) )
# 327 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | IF ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | PRINT ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | RETURN ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | WHILE ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | END ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState53 | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 381 "kawaparser.ml"
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
# 392 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
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
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | N _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | NEW ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | THIS ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | TRUE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 9 "kawaparser.mly"
       (string)
# 751 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 73 "kawaparser.mly"
                       ( Field(e, var) )
# 756 "kawaparser.ml"
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

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_goto_list_attr_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.attribute_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.attribute_def))), _, (xs : (Kawa.attribute_def list))) = _menhir_stack in
        let _v : (Kawa.attribute_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 852 "kawaparser.ml"
         in
        _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | END ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_IDENT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (
# 9 "kawaparser.mly"
       (string)
# 881 "kawaparser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 886 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) = 
# 144 "<standard.mly>"
    ( x )
# 896 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENT__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

>>>>>>> 541246b (idk anymore)
and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
<<<<<<< HEAD
    | MenhirState99 ->
=======
    | MenhirState116 ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | IDENT _v ->
                        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                    | IF ->
                        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | LPAR ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | MINUS ->
                        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | N _v ->
                        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                    | NEW ->
                        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | NOT ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | PRINT ->
                        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | RETURN ->
                        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | THIS ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | TRUE ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | WHILE ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | END ->
                        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
    | MenhirState107 ->
=======
    | MenhirState124 ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
# 82 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 218 "kawaparser.ml"
=======
# 78 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 989 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
<<<<<<< HEAD
    | MenhirState110 ->
=======
    | MenhirState127 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
<<<<<<< HEAD
# 234 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 ->
=======
# 1005 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState103 ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
# 83 "kawaparser.mly"
                                                   ( While(e, seq) )
# 250 "kawaparser.ml"
=======
# 79 "kawaparser.mly"
                                                   ( While(e, seq) )
# 1021 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
<<<<<<< HEAD
    | MenhirState131 ->
=======
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (ret : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 1042 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (loc : ((string * Kawa.typ) list list))), _, (code : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1047 "kawaparser.ml"
             in
            
# 52 "kawaparser.mly"
                                                                                                                                 ( {method_name=id; code=code; params=params; locals=List.concat loc; return=ret} )
# 1052 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | END ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState135
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
# 34 "kawaparser.mly"
                                 ( seq )
# 272 "kawaparser.ml"
=======
# 104 "kawaparser.mly"
                                 ( seq )
# 1086 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
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
# 30 "kawaparser.mly"
<<<<<<< HEAD
                                                           ( {classes=cls; globals=glb; main=main_fun} )
# 286 "kawaparser.ml"
=======
                                                           ( {classes=cls; globals=List.concat glb; main=main_fun} )
# 1100 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
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

<<<<<<< HEAD
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
=======
and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1126 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
>>>>>>> 541246b (idk anymore)

and _menhir_goto_mem : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
<<<<<<< HEAD
    | MenhirState101 | MenhirState96 | MenhirState91 | MenhirState87 | MenhirState30 | MenhirState33 | MenhirState36 | MenhirState38 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState55 | MenhirState53 | MenhirState48 | MenhirState44 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState131 | MenhirState120 | MenhirState28 | MenhirState86 | MenhirState110 | MenhirState107 | MenhirState99 ->
=======
    | MenhirState118 | MenhirState113 | MenhirState108 | MenhirState104 | MenhirState47 | MenhirState50 | MenhirState53 | MenhirState55 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState65 | MenhirState61 | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState147 | MenhirState45 | MenhirState103 | MenhirState127 | MenhirState124 | MenhirState116 ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | LPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | N _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | NEW ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | THIS ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | TRUE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
    | MenhirState39 ->
=======
    | MenhirState56 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
<<<<<<< HEAD
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
=======
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
<<<<<<< HEAD
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
=======
# 65 "kawaparser.mly"
                   ( e )
# 1223 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 91 "kawaparser.mly"
       ( Mul )
# 1247 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1252 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
<<<<<<< HEAD
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
=======
    | MenhirState53 | MenhirState92 | MenhirState65 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
<<<<<<< HEAD
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
=======
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | LPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | N _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | NEW ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | THIS ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | TRUE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
>>>>>>> 541246b (idk anymore)
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1327 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 89 "kawaparser.mly"
       ( Add )
# 1355 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1360 "kawaparser.ml"
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
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 93 "kawaparser.mly"
      ( Rem )
# 1382 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1387 "kawaparser.ml"
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
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 92 "kawaparser.mly"
      ( Div )
# 1409 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1414 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 101 "kawaparser.mly"
     ( Or )
# 1458 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1463 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 95 "kawaparser.mly"
         ( Neq )
# 1495 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1500 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 90 "kawaparser.mly"
        ( Sub )
# 1528 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1533 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 96 "kawaparser.mly"
        ( Lt )
# 1565 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1570 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 97 "kawaparser.mly"
         ( Le )
# 1602 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1607 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 98 "kawaparser.mly"
          ( Gt )
# 1639 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
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
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 99 "kawaparser.mly"
         ( Ge )
# 1676 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
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
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 94 "kawaparser.mly"
        ( Eq )
# 1713 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1718 "kawaparser.ml"
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 100 "kawaparser.mly"
      ( And )
# 1762 "kawaparser.ml"
             in
            
# 64 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1767 "kawaparser.ml"
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
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 85 "kawaparser.mly"
        ( Opp )
# 1789 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
               ( Unop(u, e) )
# 1794 "kawaparser.ml"
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
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 86 "kawaparser.mly"
      ( Not )
# 1816 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
               ( Unop(u, e) )
# 1821 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
<<<<<<< HEAD
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
=======
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | IDENT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | IF ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | LPAR ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | MINUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | N _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
                | NEW ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | NOT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | PRINT ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | RETURN ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | THIS ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | TRUE ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | WHILE ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState103
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
>>>>>>> 541246b (idk anymore)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
<<<<<<< HEAD
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
=======
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 80 "kawaparser.mly"
                     ( Return(e) )
# 1956 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
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
# 76 "kawaparser.mly"
                              ( Print(e) )
# 2011 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | IDENT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | IF ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LPAR ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | MINUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | N _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | NEW ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | NOT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | PRINT ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | RETURN ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | THIS ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | TRUE ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | WHILE ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
>>>>>>> 541246b (idk anymore)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
<<<<<<< HEAD
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
=======
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
<<<<<<< HEAD
    | MenhirState101 ->
=======
    | MenhirState118 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
<<<<<<< HEAD
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
=======
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
<<<<<<< HEAD
# 81 "kawaparser.mly"
                             ( Set(mem, e) )
# 2071 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
=======
# 77 "kawaparser.mly"
                             ( Set(mem, e) )
# 2154 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
<<<<<<< HEAD
    | MenhirState131 | MenhirState28 | MenhirState120 | MenhirState86 | MenhirState99 | MenhirState107 | MenhirState110 ->
=======
    | MenhirState147 | MenhirState45 | MenhirState103 | MenhirState116 | MenhirState124 | MenhirState127 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
<<<<<<< HEAD
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
=======
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
<<<<<<< HEAD
# 85 "kawaparser.mly"
              ( Expr(e) )
# 2121 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
=======
# 81 "kawaparser.mly"
              ( Expr(e) )
# 2204 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
>>>>>>> 541246b (idk anymore)
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
    | MenhirState16 ->
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
<<<<<<< HEAD
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
=======
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | IDENT _v ->
                    _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | IF ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | LPAR ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | MINUS ->
                    _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | N _v ->
                    _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | NEW ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | NOT ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | PRINT ->
                    _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | RETURN ->
                    _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | THIS ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | TRUE ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | WHILE ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
    | MenhirState136 ->
=======
    | MenhirState152 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
<<<<<<< HEAD
# 2213 "kawaparser.ml"
=======
# 2287 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
         in
        _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

<<<<<<< HEAD
and _menhir_goto_option_class_extension_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
=======
and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.attribute_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2298 "kawaparser.ml"
     in
    _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATTRIBUTE ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
<<<<<<< HEAD
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
=======
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
>>>>>>> 541246b (idk anymore)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

<<<<<<< HEAD
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
=======
and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
>>>>>>> 541246b (idk anymore)
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
<<<<<<< HEAD
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
=======
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27
>>>>>>> 541246b (idk anymore)

and _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
<<<<<<< HEAD
    | MenhirState23 ->
=======
    | MenhirState37 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
<<<<<<< HEAD
# 2797 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
=======
# 2362 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
<<<<<<< HEAD
# 2807 "kawaparser.ml"
=======
# 2372 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
            | VAR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | END | FALSE | IDENT _ | IF | LPAR | MINUS | N _ | NEW | NOT | PRINT | RETURN | THIS | TRUE | WHILE ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
>>>>>>> 541246b (idk anymore)
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

<<<<<<< HEAD
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
=======
and _menhir_goto_attr_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.attribute_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATTRIBUTE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | FINAL ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | END | METHOD ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENT__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), _, (xs : (string list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = let id_list = 
# 232 "<standard.mly>"
    ( xs )
# 2449 "kawaparser.ml"
         in
        
# 39 "kawaparser.mly"
                                                        ( List.map (fun id -> (id, t)) id_list )
# 2454 "kawaparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | CLASS | END | FALSE | IDENT _ | IF | LPAR | MAIN | MINUS | N _ | NEW | NOT | PRINT | RETURN | THIS | TRUE | WHILE ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 2479 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (
# 9 "kawaparser.mly"
       (string)
# 2502 "kawaparser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 2507 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2527 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | N _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | NEW ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | THIS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | TRUE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 59 "kawaparser.mly"
       ( Bool(true) )
# 2578 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 61 "kawaparser.mly"
       ( This )
# 2589 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | LPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | N _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | NEW ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | THIS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | TRUE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
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
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | IDENT _v ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LPAR ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | MINUS ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | N _v ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | NEW ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NOT ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | THIS ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | TRUE ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | AND | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 2737 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 66 "kawaparser.mly"
               ( New(id) )
# 2742 "kawaparser.ml"
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

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (int)
# 2761 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "kawaparser.mly"
       (int)
# 2769 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 58 "kawaparser.mly"
      ( Int(n) )
# 2774 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IDENT _v ->
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAR ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | N _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | NEW ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | THIS ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRUE ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | LPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | N _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | NEW ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | THIS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | TRUE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 2879 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (var : (
# 9 "kawaparser.mly"
       (string)
# 2887 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.mem_access) = 
# 72 "kawaparser.mly"
            ( Var(var) )
# 2892 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 60 "kawaparser.mly"
        ( Bool(false) )
# 2903 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2912 "kawaparser.ml"
     in
    _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ATTRIBUTE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | FINAL ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | END | METHOD ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ATTRIBUTE ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                    | FINAL ->
                        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                    | END | METHOD ->
                        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState21
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
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
=======
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState6 in
            let _v : (string list) = 
# 142 "<standard.mly>"
    ( [] )
# 3011 "kawaparser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_IDENT__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 3036 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (Kawa.attribute_def) = 
# 42 "kawaparser.mly"
                                        ( {attribute_name=id; attribute_typ=t; final=true} )
# 3041 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (t : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 3074 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (Kawa.attribute_def) = 
# 43 "kawaparser.mly"
                                  ( {attribute_name=id; attribute_typ=t; final=false} )
# 3079 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState35 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3124 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
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
    | MenhirState35 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 9 "kawaparser.mly"
       (string)
# 3155 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 55 "kawaparser.mly"
                      ( (id, typ))
# 3161 "kawaparser.ml"
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
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3191 "kawaparser.ml"
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
>>>>>>> 541246b (idk anymore)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
<<<<<<< HEAD
    | MenhirState5 ->
=======
    | MenhirState13 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : ((string * Kawa.typ) list))), _, (xs : ((string * Kawa.typ) list list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list list) = 
# 213 "<standard.mly>"
    ( x :: xs )
<<<<<<< HEAD
# 2946 "kawaparser.ml"
=======
# 3220 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
         in
        _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLASS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | MAIN ->
<<<<<<< HEAD
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
=======
            _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState16
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | IDENT _v ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | IF ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LPAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | N _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | NEW ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | NOT ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PRINT ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | RETURN ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | THIS ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | TRUE ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | WHILE ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 48 "kawaparser.mly"
       ( TVoid )
# 3283 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 47 "kawaparser.mly"
      ( TInt )
# 3294 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 3301 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (cls_name : (
# 9 "kawaparser.mly"
       (string)
<<<<<<< HEAD
# 2974 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.typ) = 
# 76 "kawaparser.mly"
                 ( TClass(cls_name) )
# 2979 "kawaparser.ml"
=======
# 3309 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.typ) = 
# 49 "kawaparser.mly"
                 ( TClass(cls_name) )
# 3314 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
<<<<<<< HEAD
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
=======
    let _v : (Kawa.typ) = 
# 46 "kawaparser.mly"
       ( TBool )
# 3325 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v
>>>>>>> 541246b (idk anymore)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
<<<<<<< HEAD
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState126 ->
=======
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
    | MenhirState110 ->
=======
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
<<<<<<< HEAD
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
=======
>>>>>>> 541246b (idk anymore)
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
    | MenhirState87 ->
=======
    | MenhirState92 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
=======
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
>>>>>>> 541246b (idk anymore)
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
    | MenhirState59 ->
=======
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
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
=======
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
>>>>>>> 541246b (idk anymore)
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
=======
    | MenhirState37 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
<<<<<<< HEAD
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
=======
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
>>>>>>> 541246b (idk anymore)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
<<<<<<< HEAD
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
=======
>>>>>>> 541246b (idk anymore)
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
<<<<<<< HEAD
    | MenhirState5 ->
=======
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
>>>>>>> 541246b (idk anymore)
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

<<<<<<< HEAD
and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
=======
and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
>>>>>>> 541246b (idk anymore)
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list list) = 
# 211 "<standard.mly>"
    ( [] )
<<<<<<< HEAD
# 3268 "kawaparser.ml"
=======
# 3532 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
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
<<<<<<< HEAD
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
=======
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState0
>>>>>>> 541246b (idk anymore)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

<<<<<<< HEAD
# 3357 "kawaparser.ml"
=======
# 3591 "kawaparser.ml"
>>>>>>> 541246b (idk anymore)
