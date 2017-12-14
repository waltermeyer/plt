(*
 * Gantry: Semantic AST
 * Author: Audrey Copeland
 * Contributors: Walter Meyer, Taimur Samee
 *)

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
     match lvaluet, rvaluet with
       (Object, _) -> lvaluet
     | (_, Object) -> lvaluet
     | (_, _)      -> if lvaluet == rvaluet then lvaluet else raise err
  in

			
  let add_local (t, n) =
    ignore(Hashtbl.add symbols n t);
  in

  let check_decl lt lv err = 
     if lt == Object then 
	(if (Hashtbl.mem symbols lv) then (raise err)
	else 
	  add_local(lt, lv))
     else 
	add_local(lt, lv);
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

  if List.mem "print_k" (List.map (fun fd -> fd.f_id) functions)
  then raise (Failure ("function print key may not be defined")) else (); 

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.f_id) functions);

  (* Function declaration for a named function *)

  (* Built-ins: print, arrify, objectify, jsonify, length, slice, tostring, httpget, httppost *)
  let built_in_decls = StringMap.add "print_s" 
     { type_spec = Null; f_id = "print_s"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "print_i"
     { type_spec = Null; f_id = "print_i"; f_params = [(Int, "x")] ; f_statements = [] }
     (StringMap.add "print_d"
     { type_spec = Null; f_id = "print_d"; f_params = [(Float, "x")] ; f_statements = [] }
     (StringMap.add "print_b"
     { type_spec = Null; f_id = "print_b"; f_params = [(Bool, "x")] ; f_statements = [] }
     (StringMap.add "print_k"
     { type_spec = Null; f_id = "print_k"; f_params = [(Object, "x")] ; f_statements = [] }
     (StringMap.add "get_string"
     { type_spec = String; f_id = "get_string"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "stoint"
     { type_spec = Int; f_id = "length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "length"
     { type_spec = Int; f_id = "length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "slice"
     { type_spec = String; f_id = "slice"; f_params = [(String, "x"); (Int, "y"); (Int, "z")] ; f_statements = [] }
     (StringMap.add "stringcmp"
     { type_spec = Int; f_id = "stringcmp"; f_params = [(String, "x"); (String, "y")] ; f_statements = [] }
     (StringMap.add "string_length"
     { type_spec = Int; f_id = "string_length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "arr_length"
     { type_spec = Int; f_id = "arr_length"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "tostring"
     { type_spec = String; f_id = "tostring"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "nap"
     { type_spec = Int; f_id = "nap"; f_params = [(Int, "x")] ; f_statements = [] }
     (StringMap.add "httpget"
     { type_spec = String; f_id = "httpget"; f_params = [(String, "x")] ; f_statements = [] }
     (StringMap.add "arr_stringify"
     { type_spec = String; f_id = "arr_stringify"; f_params = [(String , "x")] ; f_statements = [] }
     (StringMap.add "obj_stringify"
     { type_spec = String; f_id = "obj_stringify"; f_params = [(Object, "x")] ; f_statements = [] } 
     (StringMap.add "obj_addkey"
     { type_spec = Object; f_id = "obj_addkey"; f_params = [(Object, "x"); (String, "y"); (Int, "z"); (String, "a")] ; f_statements = [] } 
     (StringMap.singleton "httppost" 
     { type_spec = String; f_id = "httppost"; f_params = [(String, "x");(String, "x")] ; f_statements = [] }))))))))))))))))))
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
     Hashtbl.clear symbols;
     
     (* Check that function does not have null parameters *)
     List.iter (check_not_null (fun n -> "illegal null formal " ^ n ^            
       " in " ^ func.f_id)) func.f_params;                                       

     (* Report if there are duplicate function parameters *)                                                                                 
     report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.f_id)   
       (List.map snd func.f_params);
 
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
	| Id s -> if not (String.contains s '.') then (type_of_identifier s) else (Object)
	| Binop(e1, op, e2) as e -> let t1 = expression e1 and t2 = expression e2 in
         (match op with
            Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
          | Add | Sub | Mult | Div when t1 = Float && t2 = Float -> Float
          | Eq | Neq when t1 = t2 -> Bool
	  | Lt | Leq | Gt | Geq when t1 = Int && t2 = Int -> Bool
          | Lt | Leq | Gt | Geq when t2 = Float && t2 = Float -> Bool
          | And | Or when t1 = Bool && t2 = Bool -> Bool
	  | Conc when t1 = String && t2 = String -> String 
          | _ -> raise (Failure ("illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " "     ^ string_of_typ t2 ^ " in " ^ string_of_expression e))
 	)
	| Unop (op, e) as ex -> let t = expression e in
	 (match op with
	  Neg when t = Int -> Int
          | Neg when t = Float -> Float
	  | Not when t = Bool -> Bool
          | Inc when t = Int -> Int
	  | Dec when t = Int -> Int
	  | _ -> raise (Failure ("Illegal unary operator " ^ string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expression ex)))
	| Assign(e1, e2) as ex -> let lt = expression e1
				  and rt = expression e2 in
		check_assign lt rt (Failure("illegal assignment " ^ string_of_typ lt ^ 
		" = " ^ string_of_typ rt ^ " in " ^ string_of_expression ex))
      	| AssignDecl (t, n, e) as ex ->
		check_decl t n (Failure("Object " ^ n ^ " declared twice"));
		let lt = type_of_identifier n
		and rt = expression e in 
		if (String.contains n '.') then
			(raise (Failure ("Can not declare multi-level variable " ^ n ^ " in expression " ^ string_of_expression ex)))
		else
			(check_assign lt rt (Failure("illegal assignment " ^ string_of_typ lt ^ 
		" = " ^ string_of_typ rt ^ " in " ^ string_of_expression ex)))
	| FunExp(f_id, actuals) as funexp -> 
		let fd = function_decl f_id in
		if List.length actuals !=  List.length fd.f_params then
	 	raise (Failure ("expecting " ^ string_of_int 
		   (List.length fd.f_params) ^ " arguments in " ^ string_of_expression funexp))
		else
		  List.iter2 
		    (fun (ft, _) e -> let et = expression e in
		      let obj_addkey = Str.regexp "obj_addkey" in
		      let arr_length = Str.regexp "arr_length" in
		      let arr_stringify = Str.regexp "arr_stringify" in
		      if not (String.contains (string_of_expression e) '.' )
		      && not (Str.string_match obj_addkey f_id 0)
		      && not (Str.string_match arr_stringify f_id 0)
		      && not (Str.string_match arr_length f_id 0) then (
		      ignore (check_assign ft et 
		        (Failure ("illegal actual argument found " ^ string_of_typ et ^
		        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expression e)))
		      )
		    )
                    fd.f_params actuals;
		fd.type_spec
	| KeyVal (_, s, e) as ex ->
		let lt = type_of_identifier s 
		and rt = expression e in
		check_assign lt rt (Failure("Key " ^ string_of_typ lt ^ 
		" has different type from value " ^ string_of_typ rt ^ " in " ^ string_of_expression ex))
	| ArrExp el -> let arr_typ e =
			 match e with
			    Int    -> Int_Array
			  | Float  -> Float_Array
			  | Object -> Object_Array
			  | String -> String_Array
			  | Bool   -> Bool_Array
			  | _      -> raise (Failure("Invalid array type"))
		       in
		       let t = List.fold_left
				(fun e1 e2 ->
				  if (e1 == expression e2) then
				    e1
				  else raise
				    (Failure("Multiple types inside an array of type " ^ string_of_typ e1))
				)
		                (expression (List.hd el)) (List.tl el)
		       in
		       arr_typ t
	| ArrAcc (e1, _) -> (match (expression e1) with
			       Int_Array    -> Int
			     | Float_Array  -> Float
			     | Object_Array -> Object
			     | String_Array -> String
			     | Bool_Array   -> Bool
			     | Object	    -> Object
			     | _	    -> raise (Failure("Invalid array access on " ^
						      string_of_expression e1)))
	| ObjExp (_) -> Object
	| Noexpr -> Null
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
      | Return e -> let t = expression e in if t = func.type_spec then () else
	 raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^ 
			  string_of_typ func.type_spec ^ " in " ^ string_of_expression e))
      | If(p, s1, s2) -> check_bool_expression p; statement s1; statement s2;
      | For(e1, e2, e3, st) -> ignore (expression e1); check_bool_expression e2; ignore(expression e3); statement st
      | While(p, s) -> check_bool_expression p; statement s
     in 

     statement (Block func.f_statements)
	  
  in
  List.iter check_function functions
