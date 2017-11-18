(* Semantic checking for the Gantry compiler *)
(* TODO: debugging [check line 71], error statements, global statements vs vars?, symbol table *)

open Ast

module StringMap = Map.Make(String)


(* Create Hash Table for symbol table *)
let symbols : (string, Ast.typ) Hashtbl.t = Hashtbl.create 10;;

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Checks each global _statement_ , then check each function *)

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
  List.iter (check_not_null (fun n -> "illegal null global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)
  (* Checks to make sure print functions are defined *)
  if List.mem "print_s" (List.map (fun fd -> fd.f_id) functions)
  then raise (Failure ("function print string may not be defined")) else (); 
  
  if List.mem "print_i" (List.map (fun fd -> fd.f_id) functions)
  then raise (Failure ("function print integer may not be defined")) else (); 
  
  if List.mem "print_d" (List.map (fun fd -> fd.f_id) functions)
  then raise (Failure ("function print float may not be defined")) else (); 

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.f_id) functions);

  (* Function declaration for a named function *)

  (* Built-ins: print, arrify, objectify, jsonify, length, slice, tostring, httpget, httppost *)
  let built_in_decls = StringMap.add "print_s" 
     { type_spec = Null; f_id = "print_s"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "print_i"
     { type_spec = Null; f_id = "print_i"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "print_d"
     { type_spec = Null; f_id = "print_d"; f_params = [(String, "x")] ; f_statements = [] }
     (* TODO : Decide whether we are implementing in our language or in codegen *)
     (*(StringMap.add "arrify"
     { type_spec = Array; f_id = "arrify"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "objectify"
     { type_spec = Object; f_id = "objectify"; f_params = [(String, "x")] ; f_statements = [] } 
     (StringMap.add "jsonify"
     { type_spec = String; f_id = "jsonify"; f_params = [(Object, "x")] ; f_statements = [] }*) 
     (StringMap.add "length"
     { type_spec = Int; f_id = "length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "slice"
     { type_spec = String; f_id = "slice"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "tostring"
     { type_spec = String; f_id = "tostring"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "httpget"
     { type_spec = String; f_id = "httpget"; f_params = [(String, "x")] ; f_statements = [] } 
     (StringMap.singleton "httppost" 
     { type_spec = Bool; f_id = "httppost"; f_params = [(String, "x");(String, "x")] ; f_statements = [] })))))))
  in

  (* Add built in functions to list of function declaration list *)
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.f_id fd m) built_in_decls functions
  
  in

  (* Check that function exists in function declaration list *)
  let function_decl s = try StringMap.find s function_decls 
	with Not_found -> raise (Failure ("unrecognized function " ^ s))

  in

  (* Ensure "main is defined" *)
  let _ = function_decl "main" in 
  
  let check_function func = 

     (* Check that function does not have null parameters *)
     List.iter (check_not_null (fun n -> "illegal null formal " ^ n ^            
       " in " ^ func.f_id)) func.f_params;                                       

     (* Report if there are duplicate function parameters *)                                                                                 
     report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.f_id)   
       (List.map snd func.f_params);

     (*List.iter (check_not_null (fun n -> "illegal null local " ^ n ^             
       " in " ^ func.fname)) func.local;*)                                        
     (*
     report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.f_id)    
bals,
       (List.map snd func.locals);                                               
     *)                                                                            
     (* TODO: Figure out how to add our locals *)

     (* Add globals and function parameters to symbol table *)
     List.iter (fun (a, b) -> Hashtbl.add symbols b a) (globals @ func.f_params);

     let type_of_identifier s = 
	 try Hashtbl.find symbols s
	 with Not_found -> raise (Failure ("undeclared identifier " ^ s))
     in

     (* Return the type of an expression or throw an exception *)
     let rec expression = function
         IntLit _ -> Int
       | FloatLit _ -> Float
       | StrLit _ -> String
       | BoolLit _ -> Bool
       | Id s -> type_of_identifier s
       | Binop(e1, op, e2) as e -> let t1 = expression e1 and t2 = expression e2 in
         (match op with
           Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
         | Eq | Neq when t1 = t2 -> Bool
	 | Lt | Leq | Gt | Geq when t1 = Int && t2 = Int -> Bool (* not Int -> Int again, right? *)
         | And | Or when t1 = Bool && t2 = Bool -> Bool
         | _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " "     ^ string_of_typ t2 ^ " in " ^ string_of_expression e))
 	)
      | Unop (op, e) as ex -> let t = expression e in
	 (match op with
	   Neg when t = Int -> Int
	 | Not when t = Bool -> Bool
         | Inc when t = Int -> Int
	 | Dec when t = Int -> Int
	 	| _ -> raise (Failure ("Illegal unary operator " ^ string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expression ex)))
      (* TODO add other expressions; Inc, Dec, ObjAcc, ArrAcc, AssignDecl, ObjAssign, etc. ? *)
	| Noexpr -> Null
	| Assign(e1, e2) as ex -> let lt = expression e1
				  and rt = expression e2 in
		check_assign lt rt (Failure("illegal assignment " ^ string_of_typ lt ^ 
		" = " ^ string_of_typ rt ^ " in " ^ string_of_expression ex))
      	| AssignDecl (t, n, e) as ex -> let lt = type_of_identifier n
					and rt = expression e in 
		check_assign lt rt (Failure("illegal assignment " ^ string_of_typ lt ^ 
		" = " ^ string_of_typ rt ^ " in " ^ string_of_expression ex))
	(* TODO: Why is obj assign into an expression list 
	| ObjAssign (el, e) -> *)
	(*| FunExp(f_id, actuals) as funexp -> let fd = function_decl f_id in fd.type_spec*) 
		(*let fd = function_decl f_id in
		if List.length actuals !=  List.length fd.f_params then
	 	raise (Failure ("expecting " ^ string_of_int 
		   (List.length fd.f_params) ^ " arguments in " ^ string_of_expression call))
		else*)
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
	(*| Return e -> let t = expression e in if t = function.typ then () else 
	raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ string_of_typ func.type_spec ^ " in " ^ string_of_expression e))*)
	| If(p, s1, b1, s2, b2) -> check_bool_expression p; statement s1; check_bool_expression b1; statement s2; statement b2;
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
