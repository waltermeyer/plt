(* Semantic checking for the Gantry compiler *)
(* TODO: debugging [check line 71], error statements, global statements vs vars?, symbol table *)

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
  let check_not_null exceptf = function
      (Null, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Are two sides of assignment compatible? *)
  let check_assign lvaluet rvaluet err = 
     if lvaluet == rvaluet then lvaluet else raise err
  in

  (**** Checking Global Variables ****)
  (* TODO: Add additional checks for statements, along the lines of 138 in Gantry semant.ml *)
  List.iter (check_not_null (fun n -> "illegal null global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)
  (* Checks to make sure print function is defined *)
  if List.mem "print_s" (List.map (fun fd -> fd.f_id) functions)
  then raise (Failure ("function print may not be defined")) else (); 

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.f_id) functions);

  (* Function declaration for a named function *)
  (* TODO: How do we want to implement print? MicroC uses seperate functions for int, bool, and string *)
  (* TODO : what do default functions return? *)

(* Built-ins: print, arrify, objectify, jsonify, length, slice, tostring, httpget, httppost *)
  let built_in_decls = 
     StringMap.add "print" 
     { type_spec = Null; f_id = "print"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "arrify"
     { type_spec = Array; f_id = "arrify"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "objectify"
     { type_spec = Object; f_id = "jsonify"; f_params = [(String, "x")] ; f_statements = [] } 
     (StringMap.add "jsonify" (*TODO: what if a function can take multiple parameter types? *)
     { type_spec = String; f_id = "jsonify"; f_params = [(Object, "x")] ; f_statements = [] } 
     (StringMap.add "length"
     { type_spec = Int; f_id = "length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "slice"
     { type_spec = String; f_id = "slice"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "tostring"
     { type_spec = String; f_id = "tostring"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "httpget" (*TODO: does this actually take in a string or several different typed arguments?*)
     { type_spec = String; f_id = "httpget"; f_params = [(String, "x")] ; f_statements = [] } 
     (StringMap.add "httppost" (*TODO: 'make' error on this line;
	This expression has type
         Ast.function_decl StringMap.t -> Ast.function_decl StringMap.t
       but an expression was expected of type
         Ast.function_decl StringMap.t = Ast.function_decl Map.Make(String).t
		*)
     { type_spec = Bool; f_id = "httppost"; f_params = [(String, "x")] ; f_statements = [] }))))))))

  in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
  
  in

  let function_decl s = try StringMap.find s function_decls 
	with Not_found -> raise (Failure ("unrecognized function " ^s))

  in

  let _ = function_decl "main" in (* Ensure "main is defined" *)
  (* TODO: void/null same thing? *)
  let check_function func = 
     List.iter (check_not_null (fun n -> "illegal null formal " ^ n ^            
       " in " ^ func.fname)) func.formals;                                       
                                                                                 
     report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)   
       (List.map snd func.formals);                                              
                                                                                 
     List.iter (check_not_null (fun n -> "illegal null local " ^ n ^             
       " in " ^ func.fname)) func.locals;                                        
                                                                                 
     report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)    
       (List.map snd func.locals);                                               
                                                                                 
     (* Type of each variable (global, formal, or local *)                       
     let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)          
         StringMap.empty (globals @ func.formals @ func.locals )                 
     in                                                                          

     (* Return the type of an expression or throw an exception *)
     let rec expression = function
(* TODO: any others? *)
         IntLit _ -> Int
       | FloatLit _ -> Float
       | BoolLit _ -> Bool
       | Id s -> type_of_identifier s
       | Binop(e1, op, e2) as e -> let t1 = expression e1 and t2 = expression e2 in
         (match op with
           Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
         | Equal | Neq when t1 = t2 -> Bool
	 | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool (* not Int -> Int again, right? *)
         | And | Or when t1 = Bool && t2 = Bool -> Bool
         | _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " "     ^ string_of_typ t2 ^ " in " ^ string_of_expression e))
 	)
      | Unop (op, e) as ex -> let t = expression e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
	 	| _ -> raise (Failure ("Illegal unary operator " ^ string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expression ex)))
      (* TODO add other expressions *)
	| Noexpr -> Null
	| Assign(var, e) as ex -> let lt = type_of_identifier var
				and rt = expression e in
		check_assign lt rt (Failure("illegal assignment " ^ string_of_typ lt ^ 
		" = " ^ string_of_typ rt ^ " in " ^string_of_expression ex))
	(* TODO: microC has call... *)
      in

      let check_bool_expression e = if expression e != Bool
	then raise (Failure ("expected Boolean expression in " ^ string_of_expression e))
	else ()
	in	
      
      let rec statement = function
	  Block sl -> let rec check_block = function [Return _ as s] -> statement s
	| Return _ :: _ -> raise (Failure "nothing may follow a return")
	| Block sl :: ss -> check_block (sl @ ss)
	| s :: ss -> statement s ; check_block ss
	| [] -> ()
       in check_block sl
     | Expr e -> ignore (expression e)
     (* | Return e -> let t = expression e in if t = function.typ then () else raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.type_spec ^ " in " ^ string_of_expr e))*)
	| If(p, b1, b2) -> check_bool_expression p; statement b1; statemenet b2;
	| For(e1, e2, e3, st) -> ignore (expression e1); check_bool_expression e2;
		ignore(expression e3); statement st
	| While(p, s) -> check_bool_expression p; statement s

     in 

     statement (Block func.f_statements)
	  
  in
  List.iter check_function functions


(* TODO: Symbol table *)
        (* Symbol table examples -- 

        --EDWARDS SLIDES--

type symbol_table = {
        parent : symbol_table option;
        variables : variable_decl list
        }
let rec find_variable (scope : symbol_table) name =
        try
                List.find (fun (s, _, _, _) -> s = name) scope.variables
        with Not_found ->
        match scope.parent with
                Some(parent) -> find_variable parent name
                | _ -> raise Not_found


        --EXTEND (Fall 2016)--

type symbol = LocalVariable of int | GlobalVariable of int | FunctionParameter of int | ExtendFunction of int
and  symbolTable = symbol StringMap.t
and  symbolTableType = Locals | Globals | ExtendFunctions

...

        --SAKE--
let check_semant env fsm =
  let env' =
    let local_sym = { env.S.scope with variables = (add_local_vars fsm.S.fsm_locals env) @ env.S.scope.variables} in
    { S.scope = local_sym } in
  ignore(check_fsm_locals fsm env); ignore(check_body env' fsm)

let check program =
  let all_fsm_names = List.map (fun fsm_dec -> (fsm_dec.S.fsm_name,S.Int) ) program.S.fsms in
  let sym_tab = {S.parent = None; S.variables = all_fsm_names } in
  let env = {S.scope=sym_tab} in
  let new_syms = {sym_tab with variables = check_globals program.S.input program.S.output env} in
  let new_syms1 = {new_syms with variables = (check_pubs program.S.public env) @ (new_syms.S.variables)} in
  let env2 = { S.scope=new_syms1} in

        *)
