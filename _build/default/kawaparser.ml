
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WHILE
    | VOID
    | VAR
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
# 29 "kawaparser.ml"
  )
    | MOD
    | MINUS
    | METHOD
    | MAIN
    | LPAR
    | LOWER
    | LEQUAL
    | INT
    | IF
    | IDENT of (
# 9 "kawaparser.mly"
       (string)
# 43 "kawaparser.ml"
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
    | ATTRIBUTE
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
  | MenhirState143
  | MenhirState138
  | MenhirState133
  | MenhirState132
  | MenhirState130
  | MenhirState126
  | MenhirState118
  | MenhirState115
  | MenhirState109
  | MenhirState107
  | MenhirState104
  | MenhirState99
  | MenhirState95
  | MenhirState94
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState56
  | MenhirState52
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState28
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState18
  | MenhirState17
  | MenhirState12
  | MenhirState9
  | MenhirState1
  | MenhirState0

# 1 "kawaparser.mly"
  

  open Lexing
  open Kawa


# 129 "kawaparser.ml"

let rec _menhir_goto_class_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.class_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | MAIN ->
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_goto_list_method_def_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.method_def))), _, (xs : (Kawa.method_def list))) = _menhir_stack in
        let _v : (Kawa.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 158 "kawaparser.ml"
         in
        _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
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
# 173 "kawaparser.ml"
            ))), (parent : (
# 9 "kawaparser.mly"
       (string)
# 177 "kawaparser.ml"
            ))), _, (attrs : ((string * Kawa.typ) list))), _, (methods : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 35 "kawaparser.mly"
                                                                                                     ( {class_name=cls_name; attributes=attrs; methods=methods; parent=Some parent} )
# 182 "kawaparser.ml"
             in
            _menhir_goto_class_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
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
# 203 "kawaparser.ml"
            ))), _, (attrs : ((string * Kawa.typ) list))), _, (methods : (Kawa.method_def list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 34 "kawaparser.mly"
                                                                                ( {class_name=cls_name; attributes=attrs; methods=methods; parent=None} )
# 208 "kawaparser.ml"
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

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 225 "kawaparser.ml"
     in
    _menhir_goto_list_method_def_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 ->
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
# 264 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 269 "kawaparser.ml"
             in
            
# 67 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 274 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 295 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 300 "kawaparser.ml"
             in
            
# 66 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 305 "kawaparser.ml"
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

and _menhir_reduce8 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 61 "kawaparser.mly"
        ( Get(m) )
# 323 "kawaparser.ml"
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
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | IF ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | PRINT ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | RETURN ->
        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | WHILE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | END ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState44 | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 377 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 388 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67
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
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
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
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | LPAR ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | RPAR ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 9 "kawaparser.mly"
       (string)
# 747 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 72 "kawaparser.mly"
                       ( Field(e, var) )
# 752 "kawaparser.ml"
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

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_goto_list_attr_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | END ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 848 "kawaparser.ml"
         in
        _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | METHOD ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | END ->
            _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState133
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 ->
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
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | IDENT _v ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                    | IF ->
                        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | LPAR ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | MINUS ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | N _v ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                    | NEW ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | NOT ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | PRINT ->
                        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | RETURN ->
                        _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | THIS ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | WHILE ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | END ->
                        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
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
    | MenhirState115 ->
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
# 77 "kawaparser.mly"
                                                                                 ( If(e, seq1, seq2) )
# 954 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 970 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
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
# 78 "kawaparser.mly"
                                                   ( While(e, seq) )
# 986 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
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
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (ret : (Kawa.typ))), (id : (
# 9 "kawaparser.mly"
       (string)
# 1007 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (loc : ((string * Kawa.typ) list))), _, (code : (Kawa.seq))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1012 "kawaparser.ml"
             in
            
# 51 "kawaparser.mly"
                                                                                                                                 ( {method_name=id; code=code; params=params; locals=loc; return=ret} )
# 1017 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | METHOD ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | END ->
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState126
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
    | MenhirState138 ->
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
# 103 "kawaparser.mly"
                                 ( seq )
# 1051 "kawaparser.ml"
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
# 1065 "kawaparser.ml"
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

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1091 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_mem : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 | MenhirState104 | MenhirState99 | MenhirState95 | MenhirState38 | MenhirState41 | MenhirState44 | MenhirState46 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState63 | MenhirState61 | MenhirState56 | MenhirState52 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState138 | MenhirState36 | MenhirState94 | MenhirState118 | MenhirState115 | MenhirState107 ->
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
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | LPAR ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | AND | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | SEMI | STAR ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 64 "kawaparser.mly"
                   ( e )
# 1188 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 90 "kawaparser.mly"
       ( Mul )
# 1212 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1217 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 | MenhirState83 | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | LPAR ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1292 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 88 "kawaparser.mly"
       ( Add )
# 1320 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1325 "kawaparser.ml"
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
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 92 "kawaparser.mly"
      ( Rem )
# 1347 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1352 "kawaparser.ml"
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
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 91 "kawaparser.mly"
      ( Div )
# 1374 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1379 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 100 "kawaparser.mly"
     ( Or )
# 1423 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1428 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 94 "kawaparser.mly"
         ( Neq )
# 1460 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1465 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | OR | PLUS | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 89 "kawaparser.mly"
        ( Sub )
# 1493 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1498 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 95 "kawaparser.mly"
        ( Lt )
# 1530 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1535 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 96 "kawaparser.mly"
         ( Le )
# 1567 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1572 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 97 "kawaparser.mly"
          ( Gt )
# 1604 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1609 "kawaparser.ml"
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
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 98 "kawaparser.mly"
         ( Ge )
# 1641 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1646 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 93 "kawaparser.mly"
        ( Eq )
# 1678 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1683 "kawaparser.ml"
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 99 "kawaparser.mly"
      ( And )
# 1727 "kawaparser.ml"
             in
            
# 63 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1732 "kawaparser.ml"
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
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 84 "kawaparser.mly"
        ( Opp )
# 1754 "kawaparser.ml"
             in
            
# 62 "kawaparser.mly"
               ( Unop(u, e) )
# 1759 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 85 "kawaparser.mly"
      ( Not )
# 1781 "kawaparser.ml"
             in
            
# 62 "kawaparser.mly"
               ( Unop(u, e) )
# 1786 "kawaparser.ml"
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
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
                | IF ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | LPAR ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | PRINT ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | RETURN ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | END ->
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 79 "kawaparser.mly"
                     ( Return(e) )
# 1921 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
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
# 75 "kawaparser.mly"
                              ( Print(e) )
# 1976 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | IF ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | LPAR ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | PRINT ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | RETURN ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | END ->
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState107
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
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 76 "kawaparser.mly"
                             ( Set(mem, e) )
# 2119 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 | MenhirState36 | MenhirState94 | MenhirState107 | MenhirState115 | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 80 "kawaparser.mly"
              ( Expr(e) )
# 2169 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | IDENT _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | IF ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | LPAR ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | PRINT ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | RETURN ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | END ->
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState138
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
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
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2252 "kawaparser.ml"
         in
        _menhir_goto_list_class_def_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2269 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 2279 "kawaparser.ml"
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
            | VAR ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | END | FALSE | IDENT _ | IF | LPAR | MINUS | N _ | NEW | NOT | PRINT | RETURN | THIS | TRUE | WHILE ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
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

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 2328 "kawaparser.ml"
     in
    _menhir_goto_list_attr_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2361 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LPAR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 58 "kawaparser.mly"
       ( Bool(true) )
# 2412 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 60 "kawaparser.mly"
       ( This )
# 2423 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | LPAR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | IDENT _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LPAR ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | RPAR ->
                _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | AND | COMMA | DIV | DOT | EQUAL | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | OR | PLUS | RPAR | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 9 "kawaparser.mly"
       (string)
# 2571 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 65 "kawaparser.mly"
               ( New(id) )
# 2576 "kawaparser.ml"
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

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (int)
# 2595 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "kawaparser.mly"
       (int)
# 2603 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 57 "kawaparser.mly"
      ( Int(n) )
# 2608 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAR ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | LPAR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 2713 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (var : (
# 9 "kawaparser.mly"
       (string)
# 2721 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.mem_access) = 
# 71 "kawaparser.mly"
            ( Var(var) )
# 2726 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 59 "kawaparser.mly"
        ( Bool(false) )
# 2737 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2746 "kawaparser.ml"
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
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ATTRIBUTE ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | END | METHOD ->
                _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
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
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | END | METHOD ->
                        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState17
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
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
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
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
# 2846 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (string * Kawa.typ) = 
# 39 "kawaparser.mly"
                            ( (id, t) )
# 2851 "kawaparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                | CLASS | END | FALSE | IDENT _ | IF | LPAR | MAIN | MINUS | N _ | NEW | NOT | PRINT | RETURN | THIS | TRUE | WHILE ->
                    _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState9
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
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
    | MenhirState18 ->
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
# 2896 "kawaparser.ml"
                ))) = _menhir_stack in
                let _v : (string * Kawa.typ) = 
# 42 "kawaparser.mly"
                                  ( (id, t) )
# 2901 "kawaparser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ATTRIBUTE ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | END | METHOD ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState26 in
                    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 2958 "kawaparser.ml"
                     in
                    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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
    | MenhirState26 | MenhirState28 ->
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
# 2989 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 54 "kawaparser.mly"
                      ( (id, typ))
# 2995 "kawaparser.ml"
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
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3025 "kawaparser.ml"
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

and _menhir_goto_list_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string * Kawa.typ))), _, (xs : ((string * Kawa.typ) list))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3054 "kawaparser.ml"
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
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | IDENT _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | IF ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | LPAR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | PRINT ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | RETURN ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | END ->
            _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 47 "kawaparser.mly"
       ( TVoid )
# 3117 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 46 "kawaparser.mly"
      ( TInt )
# 3128 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "kawaparser.mly"
       (string)
# 3135 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (cls_name : (
# 9 "kawaparser.mly"
       (string)
# 3143 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.typ) = 
# 48 "kawaparser.mly"
                 ( TClass(cls_name) )
# 3148 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 45 "kawaparser.mly"
       ( TBool )
# 3159 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
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
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 211 "<standard.mly>"
    ( [] )
# 3354 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

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
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CLASS | MAIN ->
        _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3413 "kawaparser.ml"
