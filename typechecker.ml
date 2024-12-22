open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = match e with
    | Int _  -> TInt

    | Bool _ -> TBool

    | Unop Opp expr -> check expr TInt tenv; TInt
    | Unop Not expr -> check expr TBool tenv; TBool

    | Binop Eq (expr1, expr2) | Binop Neq (expr1, expr2) -> check expr1 (type_expr expr2 tenv) tenv; TBool
    | Binop And (expr1, expr2) | Binop Or (expr1, expr2) -> check expr1 TBool tenv; check expr2 TBool tenv; TBool
    | Binop (op, expr1, expr2) -> check expr1 TBool tenv; (*toutes les autres opérations binaires prennent des int*)
                              check expr2 TBool tenv;
                              match op with 
                                    Lt|Le|Gt|Ge -> Bool
                                    | _ -> TInt 

    | Get Var id -> Env.find id tenv
    | Get Field (exp, id) -> failwith "flemme"
    | _ -> failwith "flemme"


  and type_mem_access m tenv = match m with
    | _ -> failwith "case not implemented in type_mem_access"
  in

  let rec check_instr i ret tenv = match i with
    | Print e -> check e TInt tenv
    | _ -> failwith "case not implemented in check_instr"

  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s

  in

  check_seq p.main TVoid tenv
