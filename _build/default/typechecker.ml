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
  let global_env = add_env p.globals Env.empty in
 
  (*method to fetch class with proper error handling*)
  let get_class class_name = match List.find_opt (fun cls -> cls.class_name=class_name) p.classes with
                                None -> failwith "Class doesn't exist"
                              | Some cls -> cls

  in

  (*method to explore heritage tree of a class and call a given function on each class. The first time the function returns a non-None value, the exploration stops. This could be extended to allow a class to inherit from several classe at once*)
  let rec explore_heritage_tree func cls = match func cls with 
                                              Some v -> Some v 
                                            | None -> match cls.parent with 
                                                  None -> None
                                                | Some parent -> explore_heritage_tree func (List.find (fun cls -> cls.class_name=parent) p.classes)
  in

  (*method to fetch method with proper error handling*)
  let get_method class_name method_name = match explore_heritage_tree (fun cls -> List.find_opt (fun m -> m.method_name = method_name) cls.methods) (get_class class_name) with
                                            None -> failwith "class does not have this method"
                                          | Some m -> m
  in

  (*method to fetch attribute with proper error handling*)
  let get_attribute class_name attribute_name = match explore_heritage_tree (fun cls -> List.find_opt (fun attr -> attr.attribute_name = attribute_name) cls.attributes) (get_class class_name) with
                                                    None -> failwith "Class does not have this attribute"
                                                  | Some attr-> attr
  in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    match typ, typ_e with 
      TClass class_typ, TClass class_typ_e -> (match explore_heritage_tree (fun cls -> if cls.class_name = class_typ then Some class_typ else None) (get_class class_typ_e) with 
                                                  None -> type_error typ_e typ
                                                | _ -> ())

      | _ -> if typ <> typ_e then type_error typ_e typ

  and type_call cls_name method_name params =  let meth = get_method cls_name method_name in
                                                if List.for_all2 (fun (_, typ) param -> param=typ) meth.params params then (*we check the arguments are all the right type*)
                                                  meth.return

                                                else
                                                  failwith "Incorrect types for arguments given"
  
  and type_expr e tenv = match e with
    | Int _  -> TInt

    | Bool _ -> TBool

    | Unop (Opp, expr) -> check expr TInt tenv; TInt
    | Unop (Not, expr) -> check expr TBool tenv; TBool

    | Binop (Eq, expr1, expr2) | Binop (Neq, expr1, expr2) | Binop (Eq_struct, expr1, expr2) | Binop (Neq_struct, expr1, expr2) -> check expr1 (type_expr expr2 tenv) tenv; TBool
    | Binop (And, expr1, expr2) | Binop (Or, expr1, expr2) -> check expr1 TBool tenv; check expr2 TBool tenv; TBool
    | Binop (op, expr1, expr2) -> check expr1 TInt tenv; (*toutes les autres opérations binaires prennent des int*)
                              check expr2 TInt tenv;
                              (match op with 
                                    Lt|Le|Gt|Ge -> TBool
                                    | _ -> TInt)

    | Get (Var id) -> (match Env.find_opt id tenv with 
                        Some v -> v
                        | None -> match Env.find_opt id global_env with
                                     Some v -> v
                                   | None -> failwith "Variable doesn't exist")
                      

    | Get (Field (expr_obj, method_name)) -> (match type_expr expr_obj tenv with 
                                    TClass cls -> (get_attribute cls method_name).attribute_typ
                                  | _ -> failwith "Type doesn't have attributes")

    | New cls_name -> let _ = get_class cls_name in TClass cls_name
    | NewCstr (cls_name, params) -> let _ = type_call cls_name "constructor" (List.map (fun p -> type_expr p tenv) params) in TClass cls_name

    | MethCall (expr_obj, method_name, params) -> (match type_expr expr_obj tenv with
                                                     TClass cls_name -> type_call cls_name method_name (List.map (fun p -> type_expr p tenv) params)
                                                    | _ -> failwith "Trying to access method of non-object type")
                                                
    | This -> type_expr (Get (Var "this")) tenv

  in
  let rec check_instr i ret class_final_allowed tenv = match i with
    | Print e -> check e TInt tenv
    | Expr e -> check e TVoid tenv 
    | Return e -> check e ret tenv (*Technically a void function should be able to return the result of another void function*)
    | Set (Var id, expr) -> check expr (type_expr (Get (Var id)) tenv) tenv
    | Set (Field (expr_obj, method_name), expr) -> (match type_expr expr_obj tenv with 
                                                      TClass cls -> let attr = (get_attribute cls method_name) in
                                                                    check expr attr.attribute_typ tenv;
                                                                    if attr.final && not (match class_final_allowed with None -> false | Some cls_name -> cls_name = cls) then
                                                                      failwith "Can't change final attribute"

                                                      | _ -> failwith "Type doesn't have attributes")

    | If (expr, seq1, seq2) -> check expr TBool tenv; check_seq seq1 ret class_final_allowed tenv; check_seq seq2 ret class_final_allowed tenv
    | While (expr, seq) -> check expr TBool tenv; check_seq seq ret class_final_allowed tenv

  and check_seq s ret class_final_allowed tenv =
    List.iter (fun i -> check_instr i ret class_final_allowed tenv) s

  in
  check_seq p.main TVoid None Env.empty; (*we check main*)
  List.iter (fun cls -> let lenv = Env.empty in (*we check all the methods of all classes even though some might not be called during the execution*)
                        let lenv = Env.add "this" (TClass cls.class_name) lenv in
                        List.iter (fun meth ->  let lenv = List.fold_left (fun env (var_name, var_type) -> Env.add var_name var_type env) lenv meth.locals in
                                                let lenv = List.fold_left (fun env (param_name, param_type) -> Env.add param_name param_type env) lenv meth.params in
                                                if meth.method_name = "constructor" then
                                                  if meth.return != TVoid then failwith "Constructor must have type void" else
                                                  check_seq meth.code meth.return (Some cls.class_name) lenv

                                                else
                                                  check_seq meth.code meth.return None lenv;
                        ) cls.methods;
            ) p.classes
