open Kawa

exception Error of string

let error s = raise (Error s)

let type_error ty_actual ty_expected = error (Printf.sprintf "expected %s, got %s" (typ_to_string ty_expected) (typ_to_string ty_actual))

let undefined_element_error ty_el name_el = error (Printf.sprintf "the %s %s does not exist" (ty_el) (name_el))

let not_a_class_error ty_el = error (Printf.sprintf "Trying to access %s of non-object type" (ty_el))

let final_error el_name = error (Printf.sprintf "Trying to change value of final attribute : %s" (el_name))

let invalid_arguments_error method_name = error (Printf.sprintf "Invalid arguments for method %s" method_name)


module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let global_env = add_env p.globals Env.empty in (*the global environnement is accessible for all methods*)
 
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
                                            None -> undefined_element_error "method" (method_name^"("^class_name^")")
                                          | Some m -> m
  in

  (*method to fetch attribute with proper error handling*)
  let get_attribute class_name attribute_name = match explore_heritage_tree (fun cls -> List.find_opt (fun attr -> attr.attribute_name = attribute_name) cls.attributes) (get_class class_name) with
                                                    None -> undefined_element_error "attribute" (attribute_name^"("^class_name^")")
                                                  | Some attr-> attr
  in

  let rec depth_array t = match t with TArray t' -> 1 + depth_array t' | TEmptyArray -> 1 | _ -> 0 in

  let rec check_types typ_e typ = match typ_e, typ with 
                                    TClass class_typ_e, TClass class_typ -> (match explore_heritage_tree (fun cls -> if cls.class_name = class_typ then Some class_typ else None) (get_class class_typ_e) with 
                                                                                None -> type_error typ_e typ
                                                                              | _ -> ())

                                    | TArray arr1, TArray arr2 -> check_types arr1 arr2
                                    | TEmptyArray, TArray arr -> if (depth_array arr) <> 0 then type_error typ_e typ
                                    | _ -> if typ <> typ_e then type_error typ_e typ

  in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    check_types typ_e typ
    
  and type_call cls_name method_name params =  let meth = get_method cls_name method_name in
                                                List.iter2 (fun (_, typ) param -> if param<>typ then type_error typ param) meth.params params; (*we check the arguments are all the right type*)
                                                meth.return
  
  and type_expr e tenv = match e with
    | Int _  -> TInt

    | Bool _ -> TBool

    | Unop (Opp, expr) -> check expr TInt tenv; TInt
    | Unop (Not, expr) -> check expr TBool tenv; TBool

    | Binop (Eq, expr1, expr2) | Binop (Neq, expr1, expr2) | Binop (Eq_struct, expr1, expr2) | Binop (Neq_struct, expr1, expr2) -> check expr1 (type_expr expr2 tenv) tenv; TBool (*equality operations only require type homogeneity*)
    | Binop (And, expr1, expr2) | Binop (Or, expr1, expr2) -> check expr1 TBool tenv; check expr2 TBool tenv; TBool
    | Binop (op, expr1, expr2) -> check expr1 TInt tenv; (*toutes les autres opérations binaires prennent des int*)
                              check expr2 TInt tenv;
                              (match op with 
                                    Lt|Le|Gt|Ge -> TBool
                                    | _ -> TInt)

    | Get (Var id) -> (match Env.find_opt id tenv with (*first we check the local variables*)
                          Some v -> v
                        | None -> match Env.find_opt id global_env with (*then we check the global environnement*)
                                     Some v -> v
                                   | None -> undefined_element_error "variable" id)
                      

    | Get (Field (expr_obj, method_name)) -> (match type_expr expr_obj tenv with 
                                    TClass cls -> (get_attribute cls method_name).attribute_typ
                                  | _ -> not_a_class_error "attribute")

    | New cls_name -> (match cls_name with
                                 "array" -> TEmptyArray
                               | _ -> let _ = get_class cls_name in TClass cls_name)

    | NewCstr (cls_name, params) -> (match cls_name with 
                                      "array" -> (match params with 
                                                    [length; default] -> check length TInt tenv; TArray (type_expr default tenv)
                                                  | _ -> invalid_arguments_error "constructor(array)")

                                    | _ -> let _ = type_call cls_name "constructor" (List.map (fun p -> type_expr p tenv) params) in TClass cls_name)

    | MethCall (expr_obj, method_name, params) -> (match type_expr expr_obj tenv with
                                                      TClass cls_name -> type_call cls_name method_name (List.map (fun p -> type_expr p tenv) params)
                                                    | TArray t -> (match method_name with 
                                                                     "get" ->  (match params with index::[] -> check index TInt tenv; t | _ -> invalid_arguments_error "get(array)") 
                                                                   | "set" ->  (match params with [index; value] -> check index TInt tenv; check value t tenv; TVoid | _ -> invalid_arguments_error "set(array)") 
                                                                   | "copy" -> TArray t
                                                                   | "length" -> TInt
                                                                   | _ -> undefined_element_error "method" (method_name^"(array)"))

                                                    | TEmptyArray -> (match method_name with 
                                                                   | "copy" -> TEmptyArray
                                                                   | "length" -> TInt
                                                                   | _ -> undefined_element_error "method" (method_name^"(array)"))

                                                    | _ -> not_a_class_error "method")
                                                
    | This -> type_expr (Get (Var "this")) tenv
    | Array expr_list ->  if List.compare_length_with expr_list 0 = 0 then 
                            TEmptyArray 

                          else 
                            let expr_list_type = List.map (fun e -> type_expr e tenv) expr_list in
                            (*for the expression [[], [1, 2]]. The type has to be based on that of [1, 2] not necessarily that of the first element*)
                            let most_defined_element = List.fold_left (fun e1 e2 -> if depth_array e1 > depth_array e2 then e1 else e2) (List.hd expr_list_type) expr_list_type in 
                            List.iter (fun e -> check e most_defined_element tenv) expr_list;
                            TArray most_defined_element

  in
  let rec check_instr i ret class_final_allowed tenv = match i with
    | Print e -> ()
    | Expr e -> check e TVoid tenv 
    | Return e -> check e ret tenv (*we make no exception for void function as technically it makes sense for a void function to be able to return the result of another void function*)
    | Set (Var id, expr) -> check expr (type_expr (Get (Var id)) tenv) tenv (*Given that we have very simple scope management, if we can access a variable we can also set it*)
    | Set (Field (expr_obj, method_name), expr) -> (match type_expr expr_obj tenv with 
                                                      TClass cls -> let attr = (get_attribute cls method_name) in
                                                                    check expr attr.attribute_typ tenv;
                                                                    if attr.final && not (match class_final_allowed with None -> false | Some cls_name -> cls_name = cls) then
                                                                      final_error attr.attribute_name

                                                      | _ -> not_a_class_error "attribute")

    | If (expr, seq1, seq2) -> check expr TBool tenv; check_seq seq1 ret class_final_allowed tenv; check_seq seq2 ret class_final_allowed tenv
    | While (expr, seq) -> check expr TBool tenv; check_seq seq ret class_final_allowed tenv

  and check_seq s ret class_final_allowed tenv =
    List.iter (fun i -> check_instr i ret class_final_allowed tenv) s

  in
  check_seq p.main TVoid None Env.empty; (*we check main*)
  List.iter (fun cls -> let lenv = Env.empty in (*we check all the methods of all classes even though some might not be called during the execution*)
                        let lenv = Env.add "this" (TClass cls.class_name) lenv in
                        List.iter (fun meth ->  let lenv = List.fold_left (fun env (var_name, var_type) -> Env.add var_name var_type env) lenv meth.locals in (*we add the local variables to environnement*)
                                                let lenv = List.fold_left (fun env (param_name, param_type) -> Env.add param_name param_type env) lenv meth.params in (*we add the parameters to the environnement*)
                                                if meth.method_name = "constructor" then
                                                  if meth.return != TVoid then 
                                                    type_error meth.return TVoid

                                                  else
                                                    check_seq meth.code meth.return (Some cls.class_name) lenv (*if it's the constructor we're allowed to change final variables of the current object*)

                                                else
                                                  check_seq meth.code meth.return None lenv;
                        ) cls.methods;
            ) p.classes
