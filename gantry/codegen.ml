module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

let translate (globals, functions) = 
 let context = L.global_context () in
 let the_module = L.create_module context "Gantry"
(* ref ; http://llvm.org/doxygen/MachineValueType_8h_source.html - i32 i8 i1*)
 and i32_t = L.i32_type context (* 32-bit integer *)
 and i8_t = L.i8_type context   (* 8-bit integer *)
 and i1_t = L.i1_type context   (* 1-bit integer *)
 and str_t = L.pointer_type (L.i8_type context) (* pointer to 8-bit integer *)
(*
 and obj_t = L.pointer_type (L.i8_type context) (*TODO: objects *)
 and arr_t = L.pointer_type (L.i8_type context) (*TODO: arrays *)
*)
 and flt_t = L.double_type context (* double *)
 and void_t = L.void_type context in (* void *)
 
 let ltype_of_typ = function
     A.Int  -> i32_t
   | A.Float-> flt_t
(*
   | A.Object -> obj_t 
   | A.Array -> arr_t  
*)
   | A.String -> str_t 
   | A.Bool -> i1_t
   | A.Null -> void_t 
in

(* This is currently the same as the one in microC, *)
(*TODO: we also need global_stmts in the globals list*)
 let global_vars =
  let global_var m (t, n) = 
  let init = L.const_int (ltype_of_typ t) 0
  in StringMap.add n (L.define_global n init the_module) m in 
    List.fold_left global_var StringMap.empty globals
  in

(*print f, get called by built in print function *) 
 let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
 let printf_func = L.declare_function "printf" printf_t the_module in

(* TODO: Declare our built-in print function, similar to printbig in microc *)

(* Define each function (arguments and return type) so we can call it *)
(* In micro C function_decls and function_decl are unique to codegen and semant *)
 let func_decls = 
  let func_decl m fdecl =
   let name = fdecl.A.f_id
   and param_types =
    Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.f_params)
    in let ftype = L.function_type (ltype_of_typ fdecl.A.type_spec) param_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) m in
   List.fold_left func_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
 let build_function_body fdecl =
  let (the_function, _) = StringMap.find fdecl.A.f_id func_decls in
  let builder = L.builder_at_end context (L.entry_block the_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and str_format_str = L.build_global_stringptr "%s\n" "fmt" builder in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
  let local_vars =
   let add_formal m (t, n) p = L.set_value_name n p;
   let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore (L.build_store p local builder);
	StringMap.add n local m in

      let add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m in

      List.fold_left2 add_formal StringMap.empty fdecl.A.f_params
                   (Array.to_list (L.params the_function)) in
    (*
      List.fold_left add_local formals fdecl.A.locals in
    *)

    (* Return the value for a variable or formal argument *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

 (* Construct code for an expression and return the value *)
 let rec expr builder = function
 	  A.IntLit i -> L.const_int i32_t i
 	| A.FloatLit f -> L.const_float flt_t f
	| A.StrLit s -> L.build_global_stringptr s "string" builder
	| A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
	| A.Noexpr -> L.const_int i32_t 0
	| A.Id s -> L.build_load (lookup s) s builder
 	| A.Binop (e1, op, e2) -> 
	    let e1' = expr builder e1
	    and e2' = expr builder e2 in
 	    (match op with
	       A.Add 	 -> L.build_add
	     | A.Sub 	 -> L.build_sub
	     | A.Mult 	 -> L.build_mul
	     | A.Div     -> L.build_sdiv
	     | A.And 	 -> L.build_and
	     | A.Or 	 -> L.build_or
	     | A.Eq 	 -> L.build_icmp L.Icmp.Eq
	     | A.Neq 	 -> L.build_icmp L.Icmp.Ne
	     | A.Lt 	 -> L.build_icmp L.Icmp.Slt
	     | A.Leq     -> L.build_icmp L.Icmp.Sle
	     | A.Gt      -> L.build_icmp L.Icmp.Sgt
	     | A.Geq  	 -> L.build_icmp L.Icmp.Sge
	    ) e1' e2' "tmp" builder
 	| A.Unop(op, e) ->
	    let e' = expr builder e in
 	    (match op with
	       A.Neg 	-> L.build_neg
	     | A.Not 	-> L.build_not) e' "tmp" builder
	(*| A.Assign(s, e) -> let e' = expr builder e in ignore (L.build_store e' (lookup s) builder); e'*)
	| A.FunExp("print", [e]) ->
	    L.build_call printf_func [| int_format_str ; (expr builder e) |]
	    "printf" builder
	| A.FunExp("prints", [e]) ->
	    L.build_call printf_func [| str_format_str ; (expr builder e) |]
	    "prints" builder
	| A.FunExp(f, act) ->
	    let (fdef, fdecl) = StringMap.find f func_decls in
	    let actuals = List.rev (List.map (expr builder) (List.rev act)) in
	    let result = (match fdecl.A.type_spec with A.Null -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in
(*
	| A.AssignDecl (*TODO*) 
	| A.ArrAssign  (*TODO*) 
	| A.AssignObj  (*TODO*)
	| A.FunExp  (*TODO*) 
	| A.KeyVal  (*TODO*) 
	| A.ArrExp  (*TODO*) 
	| A.Noexpr  (*TODO*)
*)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder
      | A.Return e -> ignore (match fdecl.A.type_spec with
	  A.Null -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder e) builder); builder
      | A.If (predicate, then_stmt, elif_pred, elif_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 (*let elif_bool_val = expr builder elif_pred in*)
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

 	 (*let elif_bb = L.append_block context "elif" the_function in
	 add_terminal (stmt (L.builder_at_end context elif_bb) elif_stmt)
	   (L.build_br merge_bb);*)

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 (*ignore (L.build_cond_br bool_val then_bb elif_bb builder);
	 ignore (L.build_cond_br elif_bool_val elif_bb else_bb builder);*)
	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
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
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.f_statements) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.type_spec with
        A.Null -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

    List.iter build_function_body functions;
    the_module

