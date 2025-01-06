open Kawa

type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | Null

and obj = { 
  cls:    string;
  fields: (string, value) Hashtbl.t ;
}

exception Error of string
exception Return of value

let exec_prog (p: program): unit =
  let env = Hashtbl.create 16 in
  List.iter (fun (x, _) -> Hashtbl.add env x Null) p.globals;
  
  let rec eval_call f this args = (*function used to hand a method call*)
    (*we define a function to help us explore the heritage tree of a class and get the desired method. This could be generalized to allow a class to inherit from several classes*)
    let rec explore_heritage_tree cls = let cls = List.find (fun c -> cls=c.class_name) p.classes in 
                                          match List.find_opt (fun m->m.method_name=f) cls.methods with 
                                          None -> (match cls.parent with 
                                                    None -> failwith "Class doesn't have this method"
                                                    | Some parent -> explore_heritage_tree parent)

                                          | Some f -> f
    in
    let f = explore_heritage_tree this.cls in
    let lenv = Hashtbl.create 16 in (*we create the local environnement*)
    List.iter (fun (x, _) -> Hashtbl.add lenv x Null) f.locals; (*we initialize the local varables*)
    if List.length f.params  <> List.length args then failwith "Invalid parameters for the class" else
    List.iter2 (fun (param_name, _) arg -> Hashtbl.add lenv param_name arg) f.params args; (*we create the variables for the arguments*)
    Hashtbl.add lenv "this" (VObj this); (*we add the object 'this'*)
    exec_seq f.code lenv;
    match Hashtbl.find_opt lenv "return" with None -> Null | Some v -> v (*we use the pseudo-variable "return" in the local environnement to pass the return value back to the caller*)

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
                                      

    and initialize_class name = 
      let rec initialize_attributes cls ht =  List.iter (fun attr -> Hashtbl.add ht attr.attribute_name Null) cls.attributes;
                                              match cls.parent with 
                                                  None -> ()
                                                | Some parent -> initialize_attributes (List.find (fun c -> c.class_name=parent) p.classes) ht
                                          
      in
      match List.find_opt (fun cls -> cls.class_name=name) p.classes with
          None -> failwith "Class doesn't exist"
        | Some cls -> let obj = {cls=name; fields = Hashtbl.create 16} in 
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
                                          | Null, Null -> VBool true
                                          | _, _ -> VBool false)

      | Binop (Neq, expr1, expr2) -> VBool (not (evalb (Binop (Eq, expr1, expr2))))
      | Binop (Lt, expr1, expr2) -> VBool (evali(expr1) < evali(expr2))
      | Binop (Le, expr1, expr2) -> VBool (evali(expr1) <= evali(expr2))
      | Binop (Gt, expr1, expr2) -> VBool (evali(expr1) > evali(expr2))
      | Binop (Ge, expr1, expr2) -> VBool (evali(expr1) >= evali(expr2))
      | Binop (And, expr1, expr2) -> VBool (evalb(expr1) && evalb(expr2))
      | Binop (Or, expr1, expr2) -> VBool (evalb(expr1) || evalb(expr2))

      | Get (Var s) -> (match search_env_opt s with
                            None -> failwith "Variable doesn't exist"
                          | Some (_, v) -> v)

      | Get (Field (obj_expr, s)) -> let obj = evalo obj_expr in
                                      (match Hashtbl.find_opt obj.fields s with
                                        None -> failwith "Variable doesn't exist"
                                      | Some v -> v)

      | New s -> VObj (initialize_class s)

      | NewCstr (s, params) -> let obj = initialize_class s in 
                              let _ = eval_call "constructor" obj (List.map (fun expr -> eval expr) params) in (*we call the constructor method*)
                              VObj obj

      | This -> eval (Get (Var "this")) 

      | MethCall (obj_expr, method_name, params) -> eval_call method_name (evalo obj_expr) (List.map (fun expr -> eval expr) params)

    in
  
    let rec exec (i: instr): unit = match i with
      | Print e -> Printf.printf "%d\n%!" (evali e)
      | If (e, seq1, seq2) -> if evalb e then
                                exec_seq seq1
                              else
                                exec_seq seq2

      | While (e, seq) -> if evalb e then
                                let _ = exec_seq seq in
                                exec (While (e, seq))

      | Set (Var s, e) -> (match search_env_opt s with
                              None -> failwith "Can't set a variable that hasn't been declared" 
                            | Some (ht, _) -> Hashtbl.replace ht s (eval e))

      | Set (Field (obj_expr, s), e) -> let obj = evalo obj_expr in
                                      if Hashtbl.find_opt obj.fields s = None then
                                        failwith "The object doesn't have this field" 

                                      else
                                        Hashtbl.replace obj.fields s (eval e)
                              

      | Return expr ->  Hashtbl.add lenv "return" (eval expr)
      | Expr expr -> let _ = eval expr in ()


    and exec_seq s = 
      List.iter exec s

    in
    exec_seq s

  in
  exec_seq p.main (Hashtbl.create 1)