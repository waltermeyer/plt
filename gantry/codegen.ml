module L = Llvm
module A = Ast

module StringMap = Map.Make(String)


let translate (globals, functions) = 
 let context = L.global_context () in
 let the_module = L.create_module context "Gantry"
(* ref ; http://llvm.org/doxygen/MachineValueType_8h_source.html - i32 i8 i1*)
 and i32_t = L.i32_type  context
 and i8_t = L.i8_type  context
 and i1_t = L.i1_type  context
 and str_t = L.pointer_type (L.i8_type context) (*TODO: Not sure about this?*)
 and obj_t = L.pointer_type (L.i8_type context) (*TODO: Not sure about this?*)
 and flt_t = L.double_type context
 and void_t = L.void_type context in
 
 let ltype_of_typ = function
   A.Int  -> i32_t
   | A.Float-> flt_t
   | A.Object -> obj_t (*TODO: Figure out whether this is correct?*)
   | A.String -> str_t (*TODO: Figure out whether this is correct?*)
   | A.Bool -> i1_t
   | A.Null -> void_t (*LHS refers to the name in our language right?*) 
in

(* This is currently the same as the one in microC, I don't think it references anything specific to gantry *)
 let global_vars =
  let global_var m (t, n) = 
  let init = L.const_int (ltype_of_typ t) 0
  in StringMap.add n (L.define_global n init the_module) m in 
 List.fold_left global_var StringMap.empty globals

(*print f, get called by built in print function *) 
 let printf_t = L.var_arg_function_type i32_5 [| L.pointer_type i8_t |] in
 let printf_func = L.declare_function "printf" printf_t the_module in

(* TODO: Declare our built-in print function, similar to printbig in microc *)


(* Define each function (arguments and return type) so we can call it *)
(* In micro C function_decls and function_decl are unique to codegen and semant *)
 let func_decls = 
  let func_decl m fdecl =
   let name = fdec.A.fname
   and formal_types = 
    Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.formals)
    in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
   List.fold_left func_decl StringMap.empty functions in

 (* Fill in the body of the given function *)
 (* TODO : Figure out what we need to change from microC code*)


 (* Construct the function's "locals": formal arguments and locally declared variables.  Allocate each on the stack, initialize their value, if appropriate, and remember their values in the "locals" map *)
 (* TODO : Figure out what we need to change from microC code*)

 (* Construct code for an expression and return the value *)
 let rec expr builder = function
 	A.IntLit i -> L.const_int i32_t i
 	| A.FloatLit f -> L.const_int flt_t f, A.
	| A.StrLit s -> (*TODO*)
	| A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
	| A.NullLit -> L.const_int i32_t 0
 	| A.Id s -> L.build_load (lookup s) s builder (*TODO: check that this is correct*)
	| A.ArrId a -> (*TODO: Figure this out *) 
 	| A.Binop (e1, op, e2) -> 
	    let e1' = expr builder e1
	    and e2' = expr builder e2 in
 	    (match op with
 	     A.Add 	-> L.build_add
 	   | A.Sub 	-> L.build_sub
 	   | A.Mult 	-> L.build_mul
 	   | A.Div      -> L.build_sdiv
 	   | A.And 	-> L.build_and
	   | A.Or 	-> L.build_or
 	   | A.Equal 	-> L.build_icmp L.Icmp.Eq
 	   | A.Neq 	-> L.build_icmp L.Icmp.Ne
 	   | A.Less 	-> L.build_icmp L.Icmp.Slt
           | A.Leq      -> L.build_icmp L.Icmp.Sle
           | A.Greater  -> L.build_icmp L.Icmp.Sgt
	   | A.Geq  	-> L.build_icmp L.Icmp.Sge
 	   ) e1' e2' "tmp" builder
 	| A.Unop(op, e) ->
	    let e' = expr builder e in
 	    (match op with
		A.Neg 	-> L.build_neg
 		A.Not 	-> L.build_not) e' "tmp" builder
 	| A.Assign (s,e) -> let e' = expr builder e in ignore (L.build_store e' (lookup s) builder); e'
	| A.AssignDecl (*TODO*) 
	| A.ArrAssign  (*TODO*) 
	| A.ArrAssignDecl  (*TODO*) 
	| A.AssignObj  (*TODO*) 
	| A.FunExp  (*TODO*) 
	| A.KeyVal  (*TODO*) 
	| A.ArrExp  (*TODO*) 
	| A.Noexpr  (*TODO*)

(* TODO: What does this mean? *)
(* Invoke "f builder" if the current block doesn't already have a terminal (e.g., a branch). *)                             
 let add_terminal builder f =                                        
    match L.block_terminator (L.insertion_block builder) with         
      Some _ -> ()                                                    
    | None -> ignore (f builder) in 

 (*Build the code for the given statement; return the builder for the statement's successor*)
 let rec stmt builder = function
  A.Block sl -> List.fold_left stmt builder sl
  | A.Expr e -> ignore (expr builder e); builder
  | A.Return e -> ignore (match fdecl.A.typ with
      A.Null -> L.build_ret_void builder
    | _ -> L.build_ret (expr builder e) builder); builder 
  | (*TODO: A.If*)
  | A.While (predicate, body) -> (*This goes first b/c it's used by For*)
	let pred_bb = L.append_block context "while" the_function in
 	(* ignore function returns ()*)
	ignore (L.build_br pred_bb builder);

	let body_bb = L.append_block context "while_body" the_function in
 	add_terminal (stmt (L.builder_at_end context body_bb) body)
 	  (L.build_br pred_bb);

	let pred_builder = L.builder_at_end context pred_bb in
	let bool_val = expr pred_builder predicate in

 	let merge_bb = L.append_block context "merge" the_function in
	ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	L.builder_at_end context merge_bb

  | A.For (e1, e2, e3, body) -> stmt builder
	( A.Block [A. Expr e1; A.While(e2, A.Block [body; A.Expr e3]) )
  (*TODO: A.Break *)
  (*TODO: A.Continue *)
  in
  
  (*Build the code for each statement in the function*)
  (*TODO : I think our implementation will have to be different since we have statements and the function declarations , but I'm not sure whether the builder function is where that comes in*)
 let builder = stmt builder (A.Block  fdeclA.body) in
 
 (* Add a return if the last block falls off at the end*) 
(*TODO: What does this do? Copied from microC*)
  add_terminal builder (match fdecl.A.typ with                                
        A.Null -> L.build_ret_void                                              
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))                      
  in                       

  List.iter build_function_body functions;
  the_module
