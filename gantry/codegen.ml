module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* Hash Table for our function local vars *)
let f_var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;
let g_var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;

let translate (globals, functions) =

  let context = L.global_context () in
  let the_module = L.create_module context "Gantry"

    (* LLVM Types *)
    and i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and i1_t   = L.i1_type context
    and str_t  = L.pointer_type (L.i8_type context)
    (* and obj_t = L.pointer_type (L.i8_type context) TODO *)
    (* and arr_t = L.pointer_type (L.i8_type context) TODO *)
    and flt_t  = L.double_type context
    and void_t = L.void_type context in

    (* AST to LLVM types *)
    let ltype_of_typ = function
        A.Int    -> i32_t
      | A.Float  -> flt_t
      (* | A.Object -> obj_t *)
      (* | A.Array  -> arr_t *)
      | A.String -> str_t
      | A.Bool   -> i1_t
      | A.Null   -> void_t
    in



    (* Global Declarations *)
    let add_global (t, n) =
    let init = L.const_null (ltype_of_typ t) in
    Hashtbl.add g_var_tbl n (L.define_global n init the_module) in
    ignore(List.iter add_global globals);

    (* Printf Built-in *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* String Concatenation *)
    let string_concat = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let string_concat = L.declare_function "string_concat" string_concat the_module in

    (* HTTP Get built-in *)
    let httpget_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let httpget_func = L.declare_function "httpget" httpget_t the_module in

    (* Function Declarations *)
    let func_decls =
      let func_decl m fdecl =
      let name = fdecl.A.f_id
      and param_types =
      (* Just grab parameter types *)
      Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.A.f_params) in
      (* Get Function Return and Parameter Types *)
      let ftype = L.function_type (ltype_of_typ fdecl.A.type_spec) param_types in
      (* Define Function with LLVM module and store in StringMap *)
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
      (* Put Functions from AST into StringMap *)
      (* List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn. *)
      List.fold_left func_decl StringMap.empty functions in

      (* Function Body *)
      let build_function_body fdecl =
      (*
       * Search our func_decls StringMap for fdecl.A.f_id then
       * grab: L.define_function name ftype the_module for entry
       *)
      let (the_function, _) = StringMap.find fdecl.A.f_id func_decls in
      (* Create 'entry' block for the function and track using builder *)
      let builder = L.builder_at_end context (L.entry_block the_function) in

      (* Global Printf Formats *)
      let int_format_str = L.build_global_stringptr "%d\n" "int_fmt" builder
      and flt_format_str = L.build_global_stringptr "%f\n" "flt_fmt" builder
      and str_format_str = L.build_global_stringptr "%s\n" "str_fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)

      (* Function Parameters *)
      let add_param (t, n) p =
        (* Set (optional, but friendly) name of the parameter value *)
        L.set_value_name n p;
        (* Create an alloca(tion) instruction of type t to store n on stack *)
        let local = L.build_alloca (ltype_of_typ t) n builder in
        (* Insert instruction that stores paramater in a new function local *)
        ignore (L.build_store p local builder);
        (* Add formal to f_var_tbl Hash Map *)
	Hashtbl.add f_var_tbl n local in
	(* Generate and Add Function Parameters *)
        ignore(List.iter2 add_param fdecl.A.f_params
	      (Array.to_list (L.params the_function)));

      (* Function Locals *)
      let add_local (t, n) builder =
	let local = L.build_alloca (ltype_of_typ t) n builder in
	ignore(Hashtbl.add f_var_tbl n local);
      in

      (* Return the value for a local variable or a parameter *)
      let lookup n =
        try Hashtbl.find f_var_tbl n with
            Not_found -> Hashtbl.find g_var_tbl n
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
             A.Add  -> L.build_add
           | A.Sub  -> L.build_sub
           | A.Mult -> L.build_mul
           | A.Div  -> L.build_sdiv
           | A.And  -> L.build_and
           | A.Or   -> L.build_or
           | A.Eq   -> L.build_icmp L.Icmp.Eq
           | A.Neq  -> L.build_icmp L.Icmp.Ne
           | A.Lt   -> L.build_icmp L.Icmp.Slt
           | A.Leq  -> L.build_icmp L.Icmp.Sle
           | A.Gt   -> L.build_icmp L.Icmp.Sgt
           | A.Geq  -> L.build_icmp L.Icmp.Sge
        ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
             A.Neg  -> L.build_neg
           | A.Not  -> L.build_not) e' "tmp" builder
      | A.AssignDecl(t, n, e) ->
        let e' = expr builder e in
          (* First add this declaration to f_var_tbl hash map *)
          ignore (add_local (t, n) builder);
	  (* Then set it and forget it *)
          ignore (L.build_store e' (lookup n) builder);
          e'
      | A.Assign(e1, e2) ->
	(* We need to resolve expression to assign into *)
        let e2' = expr builder e2 in
          ignore (L.build_store e2' (lookup (A.expr_to_str e1)) builder);
          e2'
      | A.FunExp("print_i", [e]) ->
	L.build_call printf_func [| int_format_str ; (expr builder e) |]
	  "print_i" builder
      | A.FunExp("print_s", [e]) ->
	L.build_call printf_func [| str_format_str ; (expr builder e) |]
	  "print_s" builder
      | A.FunExp("print_d", [e]) ->
	L.build_call printf_func [| flt_format_str ; (expr builder e) |]
	  "print_d" builder
     (* | A.FunExp("httpget", [e]) ->
        L.build_call httpget_func [| str_format_str ; (expr builder e) |] *)
      | A.FunExp(f, act) ->
        let (fdef, fdecl) = StringMap.find f func_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = (match fdecl.A.type_spec with A.Null -> ""
                                            | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list actuals) result builder
      in

  (* | A.AssignDecl (*TODO*)
  | A.ArrAssign  (*TODO*)
  | A.AssignObj  (*TODO*)
  | A.FunExp  (*TODO*)
  | A.KeyVal  (*TODO*)
  | A.ArrExp  (*TODO*)
  | A.Noexpr  (*TODO*) *)

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

    (* Populate Function Bodies *)
    List.iter build_function_body functions;
    the_module
