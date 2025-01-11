open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | VArray of value array
  | Null

and obj = { 
  cls:    string;
  fields: (string, value) Hashtbl.t ;
}
exception Return of value


exception Error of string

let undefined_element_error ty_el name_el = raise (Error (Printf.sprintf "the %s %s does not exist"(ty_el) (name_el)))

let not_a_class_error ty_el = raise (Error (Printf.sprintf "Trying to access %s of non-object type" (ty_el)))

let invalid_arguments_error method_name = raise (Invalid_argument ("Invalid arguments for method "^method_name))



let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call method_name this args = (*function used to hand a method call*)
    (*we define a function to help us explore the heritage tree of a class and get the desired method. This could be generalized to allow a class to inherit from several classes*)
    let rec explore_heritage_tree cls_name method_name = let cls = List.find (fun c -> cls_name=c.class_name) p.classes in 
                                          match List.find_opt (fun m->m.method_name=method_name) cls.methods with 
                                          None -> (match cls.parent with 
                                                    None -> undefined_element_error "method" (method_name^"("^cls_name^")")
                                                    | Some parent -> explore_heritage_tree parent method_name)

                                          | Some method_name -> method_name
    in
    let meth = explore_heritage_tree this.cls method_name in (*we fetch the method in the heritage tree*)
    let lenv = Hashtbl.create 16 in (*we create the local environnement*)
    List.iter (fun (x, _) -> Hashtbl.add lenv x Null) meth.locals; (*we initialize the local varables*)
    if List.length meth.params  <> List.length args then invalid_arguments_error (method_name^"("^this.cls^")") else
    List.iter2 (fun (param_name, _) arg -> Hashtbl.add lenv param_name arg) meth.params args; (*we create the variables for the arguments*)
    Hashtbl.add lenv "this" (VObj this); (*we add the object 'this'*)
    try exec_seq meth.code lenv; Null with Return e -> e

  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false

    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false

    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false

    and search_env_opt s = match Hashtbl.find_opt lenv s with (*First we search local variables in the local environnement*)
                            Some v -> Some (lenv, v)
                            | None -> match Hashtbl.find_opt env s with (*Then we search global variables in the global environnement*)
                              | Some v -> Some (env, v)
                              | None -> None
                                      

    and initialize_class cls_name = 
      let rec initialize_attributes cls ht =  List.iter (fun attr -> Hashtbl.add ht attr.attribute_name Null) cls.attributes;
                                              match cls.parent with 
                                                  None -> ()
                                                | Some parent -> initialize_attributes (List.find (fun c -> c.class_name=parent) p.classes) ht
                                          
      in
      match List.find_opt (fun cls -> cls.class_name=cls_name) p.classes with
          None -> undefined_element_error "class" cls_name
        | Some cls -> let obj = {cls=cls_name; fields = Hashtbl.create 16} in 
          initialize_attributes cls obj.fields;
          obj
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b

      | Unop (Opp, expr) -> VInt (-(evali expr))
      | Unop (Not, expr) -> VBool (not (evalb expr))

      | Binop (Add, expr1, expr2) -> VInt (evali(expr1) + evali(expr2))
      | Binop (Sub, expr1, expr2) -> VInt (evali(expr1) - evali(expr2))
      | Binop (Mul, expr1, expr2) -> VInt (evali(expr1) * evali(expr2))
      | Binop (Div, expr1, expr2) -> VInt (evali(expr1) / evali(expr2))
      | Binop (Rem, expr1, expr2) -> VInt (evali(expr1) mod evali(expr2))
      | Binop (Eq_struct, expr1, expr2) -> VBool (eval(expr1)=eval(expr2))
      | Binop (Neq_struct, expr1, expr2) -> VBool (eval(expr1)<>eval(expr2))

      | Binop (Eq, expr1, expr2) -> (match eval(expr1), eval(expr2) with 
                                            VBool b1, VBool b2 ->  VBool (b1==b2)
                                          | VInt n1, VInt n2 ->  VBool (n1==n2)
                                          | VObj obj1, VObj obj2 -> VBool (obj1 == obj2)
                                          | VArray arr1, VArray arr2 -> VBool (arr1 == arr2)
                                          | _, _ -> VBool false)

      | Binop (Neq, expr1, expr2) -> VBool (not (evalb (Binop (Eq, expr1, expr2))))
      | Binop (Lt, expr1, expr2) -> VBool (evali(expr1) < evali(expr2))
      | Binop (Le, expr1, expr2) -> VBool (evali(expr1) <= evali(expr2))
      | Binop (Gt, expr1, expr2) -> VBool (evali(expr1) > evali(expr2))
      | Binop (Ge, expr1, expr2) -> VBool (evali(expr1) >= evali(expr2))
      | Binop (And, expr1, expr2) -> VBool (evalb(expr1) && evalb(expr2))
      | Binop (Or, expr1, expr2) -> VBool (evalb(expr1) || evalb(expr2))

      | Get (Var var_name) -> (match search_env_opt var_name with
                            None -> undefined_element_error "variable" var_name
                          | Some (_, variable) -> variable)

      | Get (Field (obj_expr, attribute_name)) -> let obj = evalo obj_expr in
                                      (match Hashtbl.find_opt obj.fields attribute_name with
                                        None -> undefined_element_error "attribute" attribute_name
                                      | Some v -> v)

      | New class_name -> (match class_name with 
                    "array" -> VArray ([||])
                  | _ -> VObj (initialize_class class_name))

      | NewCstr (class_name, params) -> (match class_name with
                                  "array" -> (match params with 
                                                [length; default] -> VArray (Array.init (evali length) (fun x -> eval default))
                                              | _ -> invalid_arguments_error ("constructor(array)"))

                                | _ -> let obj = initialize_class class_name in 
                                       let _ = eval_call "constructor" obj (List.map (fun expr -> eval expr) params) in (*we call the constructor method*)
                                       VObj obj)

      | This -> eval (Get (Var "this")) 

      | MethCall (obj_expr, method_name, params) ->   let params = List.map eval params in
                                                      (match eval obj_expr with 
                                                        VObj obj ->  eval_call method_name obj params
                                                      | VArray arr -> (match method_name with (*the methods for the array are hardcoded*)
                                                                          "set" -> (match params with [VInt index; value] -> Array.set arr index value; Null | _ -> invalid_arguments_error ("set(array)"))
                                                                        | "get" -> (match params with (VInt index)::[] -> Array.get arr index | _ -> invalid_arguments_error ("get(array)"))
                                                                        | "copy" -> (match params with [] -> VArray (Array.copy arr) | _ -> invalid_arguments_error ("copy(array)"))
                                                                        | "length" -> (match params with [] -> VInt (Array.length arr) | _ -> invalid_arguments_error ("length(array)"))
                                                                        | _ -> undefined_element_error "method" (method_name^"(array)")
                                                                      ) 

                                                      | _ -> not_a_class_error "method") 

      | Array list_exprs -> VArray (Array.of_list(List.map eval list_exprs))

    in
  
    let rec exec (i: instr): unit = match i with
      | Print e ->  let rec print_value value = match value with 
                        VInt i -> Printf.printf "%d" i
                      | VBool b -> Printf.printf "%b" b
                      | VObj obj -> Printf.printf "<Object %s>" obj.cls
                      | VArray arr -> Printf.printf "["; Array.iteri (fun i v -> if i<>0 then Printf.printf ", "; print_value v;) arr; Printf.printf "]"
                      | Null -> Printf.printf "Null"
                    in
                    print_value (eval e);
                    Printf.printf "\n%!"

      | If (expr, seq1, seq2) -> if evalb expr then
                                exec_seq seq1
                              else
                                exec_seq seq2

      | While (expr, seq) -> if evalb expr then
                                let _ = exec_seq seq in
                                exec (While (expr, seq))

      | Set (Var variable_name, expr) -> (match search_env_opt variable_name with
                              None -> undefined_element_error "variable" variable_name
                            | Some (ht, _) -> Hashtbl.replace ht variable_name (eval expr))

      | Set (Field (obj_expr, attribute_name), expr) -> let obj = evalo obj_expr in
                                      if Hashtbl.find_opt obj.fields attribute_name = None then
                                        undefined_element_error "attribute" (attribute_name^"("^obj.cls^")")

                                      else
                                        Hashtbl.replace obj.fields attribute_name (eval expr)
                              

      | Return expr ->  raise (Return (eval expr))
      | Expr expr -> let _ = eval expr in ()


    and exec_seq s = 
      List.iter exec s

    in
    exec_seq s

  in
  exec_seq p.main (Hashtbl.create 1)