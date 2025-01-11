
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
# 31 "kawaparser.ml"
  )
    | MOD
    | MINUS
    | METHOD
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
# 46 "kawaparser.ml"
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
  | MenhirState173
  | MenhirState168
  | MenhirState162
  | MenhirState156
  | MenhirState153
  | MenhirState151
  | MenhirState144
  | MenhirState143
  | MenhirState140
  | MenhirState137
  | MenhirState131
  | MenhirState125
  | MenhirState122
  | MenhirState116
  | MenhirState114
  | MenhirState111
  | MenhirState106
  | MenhirState102
  | MenhirState101
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
  | MenhirState64
  | MenhirState62
  | MenhirState57
  | MenhirState53
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState25
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

# 141 "kawaparser.ml"

let rec _menhir_reduce13 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.mem_access) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (m : (Kawa.mem_access))) = _menhir_stack in
    let _v : (Kawa.expr) = 
# 107 "kawaparser.mly"
        ( Get(m) )
# 149 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_method_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.method_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (m : (Kawa.method_def)) = _v in
    let _v : (Kawa.class_attr_metho) = 
# 49 "kawaparser.mly"
               ( Method(m) )
# 161 "kawaparser.ml"
     in
    _menhir_goto_class_attr_meth _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
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
# 181 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 186 "kawaparser.ml"
             in
            
# 113 "kawaparser.mly"
                                                                   ( MethCall(e, id, params) )
# 191 "kawaparser.ml"
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
        | R_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let exprs = 
# 232 "<standard.mly>"
    ( xs )
# 213 "kawaparser.ml"
             in
            
# 114 "kawaparser.mly"
                                                        ( Array(exprs) )
# 218 "kawaparser.ml"
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
# 8 "kawaparser.mly"
       (string)
# 239 "kawaparser.ml"
            ))), _, (xs : (Kawa.expr list))) = _menhir_stack in
            let _v : (Kawa.expr) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 244 "kawaparser.ml"
             in
            
# 112 "kawaparser.mly"
                                                            ( NewCstr(id, params) )
# 249 "kawaparser.ml"
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
    | MenhirState116 | MenhirState111 | MenhirState106 | MenhirState102 | MenhirState38 | MenhirState41 | MenhirState44 | MenhirState46 | MenhirState47 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState57 | MenhirState53 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState168 | MenhirState156 | MenhirState137 | MenhirState36 | MenhirState101 | MenhirState125 | MenhirState122 | MenhirState114 ->
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
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | LPAR ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | L_BRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | AND | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | SEMI | STAR ->
            _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState168 | MenhirState101 | MenhirState114 | MenhirState125 | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | IF ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LPAR ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | L_BRACKET ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PRINT ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | RETURN ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | WHILE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | END ->
            _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState156 | MenhirState36 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (Kawa.instr))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl) = 
# 72 "kawaparser.mly"
          ( Instr(i) )
# 363 "kawaparser.ml"
         in
        _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState44 | MenhirState47 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Kawa.expr list)) = _v in
        let _v : (Kawa.expr list) = 
# 144 "<standard.mly>"
    ( x )
# 379 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Kawa.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
        let _v : (Kawa.expr list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 390 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
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
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LPAR ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | L_BRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RPAR ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | AND | ASSIGN | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Kawa.expr))), (var : (
# 8 "kawaparser.mly"
       (string)
# 833 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.mem_access) = 
# 120 "kawaparser.mly"
                       ( Field(e, var) )
# 838 "kawaparser.ml"
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

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState114 ->
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
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | IDENT _v ->
                        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                    | IF ->
                        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | LPAR ->
                        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | L_BRACKET ->
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | MINUS ->
                        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | N _v ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
                    | NEW ->
                        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | NOT ->
                        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | PRINT ->
                        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | RETURN ->
                        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | THIS ->
                        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | TRUE ->
                        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | WHILE ->
                        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | END ->
                        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState122
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | BOOL | END | FALSE | IDENT _ | IF | INT | LPAR | L_BRACKET | MINUS | N _ | NEW | NOT | PRINT | RETURN | THIS | TRUE | VAR | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))), _, (seq1 : (Kawa.seq))) = _menhir_stack in
                let _v : (Kawa.instr) = 
# 96 "kawaparser.mly"
                                                 ( If(e, seq1, []) )
# 984 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState122 ->
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
# 1012 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr))), _, (xs : (Kawa.seq))) = _menhir_stack in
        let _v : (Kawa.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1028 "kawaparser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState101 ->
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
# 97 "kawaparser.mly"
                                                   ( While(e, seq) )
# 1044 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 ->
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
# 1066 "kawaparser.ml"
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
# 1080 "kawaparser.ml"
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
# 1117 "kawaparser.ml"
            ))), (parent : (string option))), _, (attrs_methods : (Kawa.class_attr_metho list))) = _menhir_stack in
            let _v : (Kawa.class_def) = 
# 37 "kawaparser.mly"
                                                                                                    ( (*here we get a list of both methods and attributes and we split them back. This choice means there's no constraint as far as the order : an attribute can be after a method*)
                                                                                                      let methods = List.filter_map (fun m_v -> match m_v with Method m -> Some m | _ -> None) attrs_methods in 
                                                                                                      let attrs = List.filter_map (fun m_v -> match m_v with Attr (attr) -> Some attr | _ -> None) attrs_methods in
                                                                                                      {class_name=cls_name; attributes=List.concat attrs; methods=methods; parent=parent} 
                                                                                                     )
# 1126 "kawaparser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | MAIN ->
                _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState173
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_attr_metho))), _, (xs : (Kawa.class_attr_metho list))) = _menhir_stack in
        let _v : (Kawa.class_attr_metho list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1154 "kawaparser.ml"
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
    | ATTRIBUTE ->
        _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | FINAL ->
        _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | METHOD ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | END ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState162
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_goto_list_instr_var_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.instr_var_decl list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (ret : (Kawa.typ))), _, (id : (
# 8 "kawaparser.mly"
       (string)
# 1204 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (instr_var_decls : (Kawa.instr_var_decl list))) = _menhir_stack in
            let x = () in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1210 "kawaparser.ml"
             in
            let _1 = 
# 126 "<standard.mly>"
    ( Some x )
# 1215 "kawaparser.ml"
             in
            
# 58 "kawaparser.mly"
                                                                                                                                           ( (*we use the same trick for variables and instructions in method_def as we did for attributes and methods in class_def*)
                                                                                                                             let loc = List.filter_map (fun v_i -> match v_i with Var_decl vars -> Some vars | _ -> None) instr_var_decls in
                                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                                             {method_name=id; code=code; params=params; locals=List.concat loc; return=ret}
                                                                                                                           )
# 1224 "kawaparser.ml"
             in
            _menhir_goto_method_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.instr_var_decl))), _, (xs : (Kawa.instr_var_decl list))) = _menhir_stack in
        let _v : (Kawa.instr_var_decl list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1240 "kawaparser.ml"
         in
        _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState156 ->
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
# 1255 "kawaparser.ml"
            ))), _, (xs : ((string * Kawa.typ) list))), _, (instr_var_decls : (Kawa.instr_var_decl list))) = _menhir_stack in
            let _v : (Kawa.method_def) = let params = 
# 232 "<standard.mly>"
    ( xs )
# 1260 "kawaparser.ml"
             in
            let _1 = 
# 124 "<standard.mly>"
    ( None )
# 1265 "kawaparser.ml"
             in
            
# 58 "kawaparser.mly"
                                                                                                                                           ( (*we use the same trick for variables and instructions in method_def as we did for attributes and methods in class_def*)
                                                                                                                             let loc = List.filter_map (fun v_i -> match v_i with Var_decl vars -> Some vars | _ -> None) instr_var_decls in
                                                                                                                             let code = List.filter_map (fun v_i -> match v_i with Instr i -> Some i | _ -> None) instr_var_decls in
                                                                                                                             {method_name=id; code=code; params=params; locals=List.concat loc; return=ret}
                                                                                                                           )
# 1274 "kawaparser.ml"
             in
            _menhir_goto_method_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.expr list) = 
# 142 "<standard.mly>"
    ( [] )
# 1291 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce61 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 1298 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (var : (
# 8 "kawaparser.mly"
       (string)
# 1304 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.mem_access) = 
# 119 "kawaparser.mly"
            ( Var(var) )
# 1309 "kawaparser.ml"
     in
    _menhir_goto_mem _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 110 "kawaparser.mly"
                   ( e )
# 1360 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
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
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 131 "kawaparser.mly"
       ( Mul )
# 1384 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1389 "kawaparser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 | MenhirState47 | MenhirState88 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | LPAR ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | L_BRACKET ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | MINUS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | N _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | NEW ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NOT ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | THIS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | RPAR | R_BRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 1470 "kawaparser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
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
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 129 "kawaparser.mly"
       ( Add )
# 1498 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1503 "kawaparser.ml"
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
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 133 "kawaparser.mly"
      ( Rem )
# 1525 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1530 "kawaparser.ml"
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
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 132 "kawaparser.mly"
      ( Div )
# 1552 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1557 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 141 "kawaparser.mly"
     ( Or )
# 1605 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1610 "kawaparser.ml"
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
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 143 "kawaparser.mly"
                ( Neq_struct )
# 1642 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1647 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 130 "kawaparser.mly"
        ( Sub )
# 1675 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1680 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 135 "kawaparser.mly"
         ( Neq )
# 1712 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1717 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 136 "kawaparser.mly"
        ( Lt )
# 1749 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1754 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 137 "kawaparser.mly"
         ( Le )
# 1786 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1791 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 138 "kawaparser.mly"
          ( Gt )
# 1823 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1828 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 139 "kawaparser.mly"
         ( Ge )
# 1860 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1865 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 142 "kawaparser.mly"
               ( Eq_struct )
# 1897 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1902 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | NEQUAL | NEQUAL_STRUCT | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 134 "kawaparser.mly"
        ( Eq )
# 1934 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1939 "kawaparser.ml"
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
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | OR | RPAR | R_BRACKET | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Kawa.expr))), _, (e2 : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let b = 
# 140 "kawaparser.mly"
      ( And )
# 1987 "kawaparser.ml"
             in
            
# 109 "kawaparser.mly"
                        ( Binop(b, e1, e2) )
# 1992 "kawaparser.ml"
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 124 "kawaparser.mly"
        ( Opp )
# 2014 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
               ( Unop(u, e) )
# 2019 "kawaparser.ml"
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
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIV | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.expr) = let u = 
# 125 "kawaparser.mly"
      ( Not )
# 2041 "kawaparser.ml"
             in
            
# 108 "kawaparser.mly"
               ( Unop(u, e) )
# 2046 "kawaparser.ml"
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
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | IDENT _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | IF ->
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | LPAR ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | L_BRACKET ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | PRINT ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | RETURN ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | END ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState101
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 98 "kawaparser.mly"
                     ( Return(e) )
# 2191 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
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
# 2250 "kawaparser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | IDENT _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                | IF ->
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | LPAR ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | L_BRACKET ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | PRINT ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | RETURN ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | END ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (mem : (Kawa.mem_access))), _, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 94 "kawaparser.mly"
                             ( Set(mem, e) )
# 2403 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 | MenhirState156 | MenhirState36 | MenhirState137 | MenhirState101 | MenhirState114 | MenhirState122 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL_STRUCT ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | GEQUAL ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | LEQUAL ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | LOWER ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | NEQUAL_STRUCT ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Kawa.expr))) = _menhir_stack in
            let _v : (Kawa.instr) = 
# 99 "kawaparser.mly"
              ( Expr(e) )
# 2457 "kawaparser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | STAR ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2476 "kawaparser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 2483 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_attr_metho list) = 
# 211 "<standard.mly>"
    ( [] )
# 2495 "kawaparser.ml"
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

and _menhir_run140 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState140
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run143 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | FINAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState143 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_goto_attr_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.attribute_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (attr : (Kawa.attribute_def list)) = _v in
    let _v : (Kawa.class_attr_metho) = 
# 50 "kawaparser.mly"
                 ( Attr(attr) )
# 2583 "kawaparser.ml"
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
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | IDENT _v ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | IF ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | PRINT ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | RETURN ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | VAR ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | WHILE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | END ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.instr_var_decl list) = 
# 211 "<standard.mly>"
    ( [] )
# 2642 "kawaparser.ml"
     in
    _menhir_goto_list_instr_var_decl_ _menhir_env _menhir_stack _menhir_s _v

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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LPAR ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | L_BRACKET ->
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

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | IDENT _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 104 "kawaparser.mly"
       ( Bool(true) )
# 2714 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 106 "kawaparser.mly"
       ( This )
# 2725 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | LPAR ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | L_BRACKET ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | L_BRACKET ->
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
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | IDENT _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | LPAR ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | L_BRACKET ->
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
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | AND | COMMA | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | RPAR | R_BRACKET | SEMI | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (id : (
# 8 "kawaparser.mly"
       (string)
# 2881 "kawaparser.ml"
            ))) = _menhir_stack in
            let _v : (Kawa.expr) = 
# 111 "kawaparser.mly"
               ( New(id) )
# 2886 "kawaparser.ml"
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
# 7 "kawaparser.mly"
       (int)
# 2905 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "kawaparser.mly"
       (int)
# 2913 "kawaparser.ml"
    )) = _v in
    let _v : (Kawa.expr) = 
# 103 "kawaparser.mly"
      ( Int(n) )
# 2918 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | L_BRACKET ->
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | L_BRACKET ->
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
    | R_BRACKET ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAR ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | L_BRACKET ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MINUS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | N _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | NEW ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | THIS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | IDENT _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | LPAR ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | L_BRACKET ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | N _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | NEW ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | NOT ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | THIS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 3062 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _ | L_BRACKET ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)
    | AND | ASSIGN | DIV | DOT | EQUAL | EQUAL_STRUCT | GEQUAL | GREATER | LEQUAL | LOWER | MINUS | MOD | NEQUAL | NEQUAL_STRUCT | OR | PLUS | SEMI | STAR ->
        _menhir_reduce61 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.expr) = 
# 105 "kawaparser.mly"
        ( Bool(false) )
# 3087 "kawaparser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

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
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | IDENT _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | IF ->
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | LPAR ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | L_BRACKET ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | PRINT ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | RETURN ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | END ->
                    _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState168
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
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
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Kawa.class_def))), _, (xs : (Kawa.class_def list))) = _menhir_stack in
        let _v : (Kawa.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 3162 "kawaparser.ml"
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
        | ATTRIBUTE ->
            _menhir_run143 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | BOOL ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | FINAL ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | IDENT _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
        | INT ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | METHOD ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | VOID ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | END ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState23
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
# 3217 "kawaparser.ml"
        ))) = _menhir_stack in
        let _v : (string list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3222 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState151 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ids : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 80 "kawaparser.mly"
                                                       ( List.map (fun id -> (id, typ)) ids )
# 3233 "kawaparser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState156 | MenhirState137 | MenhirState36 | MenhirState131 | MenhirState0 | MenhirState5 ->
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
# 3250 "kawaparser.ml"
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
                        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState5
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
                | MenhirState131 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                    let x = () in
                    let _v : (Kawa.instr_var_decl) = let _1 = 
# 126 "<standard.mly>"
    ( Some x )
# 3281 "kawaparser.ml"
                     in
                    
# 71 "kawaparser.mly"
                             ( Var_decl(vars) )
# 3286 "kawaparser.ml"
                     in
                    _menhir_goto_instr_var_decl _menhir_env _menhir_stack _menhir_s _v
                | MenhirState156 | MenhirState137 | MenhirState36 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                    let _v : (Kawa.instr_var_decl) = let _1 = 
# 124 "<standard.mly>"
    ( None )
# 3296 "kawaparser.ml"
                     in
                    
# 71 "kawaparser.mly"
                             ( Var_decl(vars) )
# 3301 "kawaparser.ml"
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
        | MenhirState140 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let _v : (Kawa.attribute_def list) = let _1 = 
# 124 "<standard.mly>"
    ( None )
# 3325 "kawaparser.ml"
                 in
                
# 54 "kawaparser.mly"
                                                     (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=true}) vars)
# 3330 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState144 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _), _, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let x = () in
                let _v : (Kawa.attribute_def list) = let _1 = 
# 126 "<standard.mly>"
    ( Some x )
# 3353 "kawaparser.ml"
                 in
                
# 54 "kawaparser.mly"
                                                     (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=true}) vars)
# 3358 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState143 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let x = () in
                let _v : (Kawa.attribute_def list) = let _1 = 
# 126 "<standard.mly>"
    ( Some x )
# 3381 "kawaparser.ml"
                 in
                
# 53 "kawaparser.mly"
                                               (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=false}) vars)
# 3386 "kawaparser.ml"
                 in
                _menhir_goto_attr_decl _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState162 | MenhirState23 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (vars : ((string * Kawa.typ) list))) = _menhir_stack in
                let _v : (Kawa.attribute_def list) = let _1 = 
# 124 "<standard.mly>"
    ( None )
# 3408 "kawaparser.ml"
                 in
                
# 53 "kawaparser.mly"
                                               (List.map (fun var -> let v, t = var in {attribute_name=v; attribute_typ=t; final=false}) vars)
# 3413 "kawaparser.ml"
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

and _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | FALSE ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | IDENT _v ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
                | IF ->
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | LPAR ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | L_BRACKET ->
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
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | RETURN ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | VAR ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | END ->
                    _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState36
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
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
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | FALSE ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | IDENT _v ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | IF ->
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | LPAR ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | L_BRACKET ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | MINUS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | N _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | NEW ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | NOT ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | PRINT ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | RETURN ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | THIS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | TRUE ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | VAR ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | WHILE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState156
                | END ->
                    _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState156
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Kawa.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 3577 "kawaparser.ml"
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
# 3605 "kawaparser.ml"
                )) = _v in
                let _v : (string) = 
# 45 "kawaparser.mly"
                       ( parent )
# 3610 "kawaparser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 3618 "kawaparser.ml"
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
# 3631 "kawaparser.ml"
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

and _menhir_reduce68 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3655 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 8 "kawaparser.mly"
       (string)
# 3661 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (string list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3666 "kawaparser.ml"
     in
    _menhir_goto_separated_nonempty_list_COMMA_IDENT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3673 "kawaparser.ml"
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

and _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Kawa.typ) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Kawa.typ) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
        let _v : ((string * Kawa.typ) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 3697 "kawaparser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Kawa.typ) list)) = _v in
        let _v : ((string * Kawa.typ) list) = 
# 144 "<standard.mly>"
    ( x )
# 3707 "kawaparser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list) = 
# 142 "<standard.mly>"
    ( [] )
# 3718 "kawaparser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_typed_variable__ _menhir_env _menhir_stack _menhir_s _v

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
# 3736 "kawaparser.ml"
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
# 3749 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
    | SEMI ->
        _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
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
# 3778 "kawaparser.ml"
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
            _menhir_reduce48 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "kawaparser.mly"
       (string)
# 3800 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (cls_name : (
# 8 "kawaparser.mly"
       (string)
# 3806 "kawaparser.ml"
    ))) = _menhir_stack in
    let _v : (Kawa.typ) = 
# 88 "kawaparser.mly"
                 ( TClass(cls_name) )
# 3811 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_type_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Kawa.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState156 | MenhirState143 | MenhirState144 | MenhirState140 | MenhirState137 | MenhirState36 | MenhirState131 | MenhirState0 | MenhirState5 ->
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
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState25 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | RPAR ->
                    _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState27
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | MenhirState153 | MenhirState27 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState30 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (id : (
# 8 "kawaparser.mly"
       (string)
# 3888 "kawaparser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s, (typ : (Kawa.typ))) = _menhir_stack in
            let _v : (string * Kawa.typ) = 
# 66 "kawaparser.mly"
                     ( (id, typ) )
# 3894 "kawaparser.ml"
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Kawa.typ))) = _menhir_stack in
                let _v : ((string * Kawa.typ) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3924 "kawaparser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_typed_variable_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState162 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState151 in
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                | IDENT _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
                | INT ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                | VOID ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                | RPAR ->
                    _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
            | SEMI ->
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | L_BRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState151
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState173 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState162 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
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
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState30 ->
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
    | MenhirState25 ->
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

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Kawa.typ) list list) = 
# 211 "<standard.mly>"
    ( [] )
# 4212 "kawaparser.ml"
     in
    _menhir_goto_list_var_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 87 "kawaparser.mly"
       ( TVoid )
# 4223 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 86 "kawaparser.mly"
      ( TInt )
# 4234 "kawaparser.ml"
     in
    _menhir_goto_type_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "kawaparser.mly"
       (string)
# 4241 "kawaparser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Kawa.typ) = 
# 85 "kawaparser.mly"
       ( TBool )
# 4255 "kawaparser.ml"
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
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4301 "kawaparser.ml"
