(* Semantic checking for the Gantry compiler *)

open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Checks each global _statement_ , then check each function *)

(*TODO: We have global statements as opposed to just global variables *)

let check (globals, functions) = 

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
        n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Check not void, used for functions and variables *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Are two sides of assignment compatible? *)
  let check_assign lvaluet rvaluet err = 
     if lvaluet == rvaluet then lvaluet else raise err
  in

  (**** Checking Global Variables ****)
  (* TODO: Add additional checks for statements, along the lines of 138 in Gantry semant.ml *)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)
  (* TODO : This will need to be modified from micro C because we don't have main, microC checks that main is defined, but I can't see it referenced anywhere else. I think lines 59-67 put the main function at the front of the function_decls list? *)
  (* Checks to make sure print function is defined *)
  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else (); 

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  (* TODO: How do we want to implement print? MicroC uses seperate functions for int, bool, and string*) 
  (* TODO : what do default functions return?*)
  let built_in_decls = 
     StringMap.add "print" 
     { typ_spec = Void; f_id = "print"; f_params = [(String, "x")] ; f_statements = [] }
     (*TODO: Add array to type_spec *)
     (StringMap.add "arrify" (*TODO: This should return an array but that's not a type*)
     { type_spec = Array; f_id = "arrify"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "objectify"
     { type_spec = Object; f_id = "jsonify"; f_params = [(String, "x")] ; f_statements = [] } 
     (StringMap.add "jsonify" (*TODO: Can this take in an object or an array*)
     { type_spec = String; f_id = "jsonify"; f_params = [(Object, "x")] ; f_statements = [] } 
     (StringMap.add "length"
     { type_spec = Int; f_id = "length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "slice"
     { type_spec = String; f_id = "slice"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "tostring"
     { type_spec = String; f_id = "tostring"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "httpget" (*TODO: does this actually take in a string or several different typed arguments?*)
     { type_spec = String; f_id = "httpget"; f_params = [(String, "x")] ; f_statements = [] i} 
     (StringMap.add "httppost"
     { type_spec = Bool; f_id = "httppost"; f_params = [(String, "x")] ; f_statements = [] })))))))

  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
  
  in

  let function decl s = try StringMap.find s function_decls 

  in

  (*TODO: here, micro c has a check for main, we don't have main *)
  
  let check_function func = 
     List.iter (check_not_void (fun n -> "illegal void formal " ^ n ^            
       " in " ^ func.fname)) func.formals;                                       
                                                                                 
     report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)   
       (List.map snd func.formals);                                              
                                                                                 
     List.iter (check_not_void (fun n -> "illegal void local " ^ n ^             
       " in " ^ func.fname)) func.locals;                                        
                                                                                 
     report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)    
       (List.map snd func.locals);                                               
                                                                                 
     (* Type of each variable (global, formal, or local *)                       
     let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)          
         StringMap.empty (globals @ func.formals @ func.locals )                 
     in                                                                          

     (* Return the type of an expression or throw an exception *)
     let ret expr = function
         IntLit _ -> Int
       | FloatLit _ -> Float
       | BoolLit _ -> Bool
       | Id s -> type_of_identifier s
       | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
         (match op with
           Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
         | Equal | Neq when t1 = t2 -> Bool
	 | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Int
         | And | Or when t1 = Bool && t2 = Bool -> Bool
         | _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " "     ^ string_of_typ t2 ^ " in " ^ string_of_expr e))
 	)
      (*TODO: unop*)
      (*TODO: add other expressions*)
      in
      (* TODO: Verify Statements*)
  in
  List.iter check_function functions
