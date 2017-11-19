module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* Hash Tables for our variable bindings *)
let g_var_tbl  : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;
let f_var_tbl  : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;
let kv_var_tbl : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;

(* Stack and Buffer for building Objects *)
let kv_stack = Stack.create ();;
let key_buf  = Buffer.create 10;;

let translate (globals, functions) =

  let context = L.global_context () in
  let the_module = L.create_module context "Gantry"

    (* LLVM Types *)
    and i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and b_t   = L.i8_type context
    and str_t  = L.pointer_type (L.i8_type context)
    (* and arr_t = L.pointer_type (L.i8_type context) TODO *)
    and flt_t  = L.double_type context
    and void_t = L.void_type context in
    (* Object Type *)
    let obj_t = let name = L.named_struct_type context "obj" in
                let body =
                  [|
                    L.pointer_type name; (* next *)
                    str_t; (* key *)
                    (* values *)
                    i32_t; (* value type *)
                    i32_t; (* int *)
                    flt_t; (* float *)
                    L.pointer_type name; (* child (object) *)
                    str_t; (* string *)
                    b_t;  (* bool *)
                  |] in
                  ignore (L.struct_set_body name body true);
                  name
    in

    (* AST to LLVM types *)
    let ltype_of_typ = function
        A.Int    -> i32_t
      | A.Float  -> flt_t
      | A.Object -> L.pointer_type obj_t
      (* | A.Array  -> arr_t *)
      | A.String -> str_t
      | A.Bool   -> b_t
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
    let string_concat_t = L.var_arg_function_type str_t [| L.pointer_type i8_t |] in
    let string_concat = L.declare_function "string_concat" string_concat_t the_module in

    (* String Slice *)
    let slice_t = L.var_arg_function_type str_t [| L.pointer_type i8_t ; i32_t ; i32_t |] in
    let slice = L.declare_function "slice" slice_t the_module in
    
    (* String Comparison *)
    let stringcmp_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
    let stringcmp = L.declare_function "stringcmp" stringcmp_t the_module in

    (* HTTP GET built-in *)
    let httpget_t = L.var_arg_function_type str_t [| L.pointer_type i8_t |] in
    let httpget_func = L.declare_function "httpget" httpget_t the_module in

    (* HTTP POST built-in *)
    let httppost_t = L.var_arg_function_type str_t [| L.pointer_type i8_t ; 
							L.pointer_type i8_t |] in
    let httppost_func = L.declare_function "httppost" httppost_t the_module in

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
        (* Insert instruction that stores parameter in a new function local *)
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

      (* Object Keys *)
      let add_kv (key_n, key_v) =
	ignore(Hashtbl.add kv_var_tbl key_n key_v);
      in

      (* Variable Lookup *)
      let lookup n =
        try Hashtbl.find f_var_tbl n with
            Not_found -> Hashtbl.find g_var_tbl n
      in

      (* Move a new object's keys to local or globals table *)
      let parent_keys n =
	(* Determine what table we should add to *)
	let tbl =
	  try
	    ignore(Hashtbl.find f_var_tbl n);
	    f_var_tbl
	  with
	    Not_found -> ignore(Hashtbl.find g_var_tbl n);
	    g_var_tbl
	in
	(* Add each orphan with 'n' as a parent to tbl *)
	let add k v =
	  let k = (n ^ k) in
	    Hashtbl.add tbl k v;
	in
	ignore(Hashtbl.iter add kv_var_tbl);
	ignore(Hashtbl.reset kv_var_tbl);
      in

    (* Construct code for an expression and return the value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float flt_t f
      | A.StrLit s -> L.build_global_stringptr s "string" builder
      | A.BoolLit b -> L.const_int b_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.Binop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
        (match op with
             A.Add  -> L.build_add e1' e2' "tmp" builder
           | A.Sub  -> L.build_sub e1' e2' "tmp" builder
           | A.Mult -> L.build_mul e1' e2' "tmp" builder
           | A.Div  -> L.build_sdiv e1' e2' "tmp" builder
           | A.And  -> L.build_and e1' e2' "tmp" builder
           | A.Or   -> L.build_or e1' e2' "tmp" builder
           | A.Eq   -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
           | A.Neq  -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
           | A.Lt   -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
           | A.Leq  -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
           | A.Gt   -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
           | A.Geq  -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
           | A.Conc -> L.build_call string_concat [| e1'; e2'|]
		       "string_concat" builder
        )
      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
             A.Neg  -> L.build_neg e' "tmp" builder
           | A.Not  -> L.build_not e' "tmp" builder
	   | A.Inc  ->
	       let n   = lookup (A.expr_to_str e) in
	       let tmp = L.build_load n "tmp" builder in
	       let tmp = L.build_add (L.const_int i32_t 1) tmp "tmp" builder in
	       L.build_store tmp n builder
	   | A.Dec  ->
	       let n   = lookup (A.expr_to_str e) in
	       let tmp = L.build_load n "tmp" builder in
	       let tmp = L.build_sub tmp (L.const_int i32_t 1) "tmp" builder in
	       L.build_store tmp n builder
	)
      | A.KeyVal(t, n, e) ->
	(* Resolve struct index and 'value_typ' in struct *)
	let sidx_of_typ = function
	    A.Int    -> 3 
	  | A.Float  -> 4
	  | A.Object -> 5
	  (* | A.Array  -> arr_t *)
	  | A.String -> 6
	  | A.Bool   -> 7
	  | A.Null   -> 8
        in
	(* Set object ID prefix for key e.g. '.b.c.d' *)
	let set_obj t n =
	  match t with
	    (* This key is an object indicate nesting using a dot *)
	    A.Object -> ignore(Buffer.add_string key_buf ("." ^ n));
	  | _        -> ()
	in
	ignore(set_obj t n);
	(* Set Key Name *)
	let set_kn kn =
	  (Buffer.contents key_buf) ^ "." ^ kn
	in
	(* Build the data structure for a key *)
	let e' = expr builder e in
	let k = L.build_malloc obj_t "obj" builder in
	(* Set key name *)
	let name = L.build_global_stringptr n "key_name_str" builder in
	ignore(L.build_store name
	(L.build_struct_gep k 1 "key_name" builder) builder);
	(* Set value type *)
	ignore(L.build_store (L.const_int i32_t (sidx_of_typ t))
	(L.build_struct_gep k 2 "value_typ" builder) builder);
	(* Set value *)
	let value = L.build_struct_gep k (sidx_of_typ t) "value" builder in
	ignore(L.build_store e' value builder);
	(* Add the key value pair to a stack *)
	ignore(Stack.push (sidx_of_typ t, (set_kn n), k) kv_stack);
        e'
      | A.ObjExp(el) ->
	(* Set next for a key or object *)
	let set_next k =
	try
	  let (_, _, next_k) = Stack.top kv_stack in
	    ignore(L.build_store next_k
	          (L.build_struct_gep k 0 "next" builder) builder);
	with
	  Stack.Empty ->
	    (* NULL *) 
	    ignore(L.build_store
		  (L.const_pointer_null (L.pointer_type obj_t))
	          (L.build_struct_gep k 0 "next" builder) builder);
	in
	(* Resolve the list of key value exprs (putting them into a stack) *)
	let kv = List.map (expr builder) el in
	(* The Enclosing (parent) Object *)
	let parent = L.build_malloc obj_t "obj" builder in
	(* Connect parent to first key *)
	ignore(set_next parent);
	(* Connect each key in this object and add to lookup table *)
	let build_obj _ =
	  let (t, n, k) = Stack.pop kv_stack in
	  (* Add parent and its key to tmp object lookup table *)
	  let value = (L.build_struct_gep k t "value" builder) in
	  (* Add orphan keys to tmp kv table *)
	  ignore(add_kv (n, value));
	  ignore(set_next k);
	in
	(* Build out all of the key values within this object *)
	List.iter build_obj kv;
	(* Prune the Object ID buffer *)
	let s = Buffer.contents key_buf in
	let s = Str.split (Str.regexp_string ".") s in
	let s =
	  try
	    List.tl (List.rev s)
	  with
	    Failure("tl") -> []
	in
	let s = String.concat "" s in
	ignore(Buffer.reset key_buf);
	ignore(Buffer.add_string key_buf s);
	(* Return the pointer to the enclosing object *)
	parent
      | A.AssignDecl(t, n, e) ->
        let e' = expr builder e in
          (* First add this declaration to f_var_tbl hash map *)
          ignore (add_local (t, n) builder);
	  (* Then set it and forget it *)
          ignore (L.build_store e' (lookup n) builder);
	  (* If this was an object, 'parent' the orphan keys *)
	  ignore(parent_keys n);
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
      | A.FunExp("httpget", [e]) ->
        L.build_call httpget_func [| (expr builder e) |]
          "httpget" builder
      | A.FunExp("httppost", [e; e2]) ->
          let e2' = expr builder e2 in
            L.build_call httppost_func [| (expr builder e) ; (e2') |]
            "httppost" builder
      | A.FunExp("slice", [e; e1; e2]) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
	    L.build_call slice [| (expr builder e) ; (e1') ; (e2') |]
	    "slice" builder
      | A.FunExp("stringcmp", [e1; e2]) ->
          let e1' = expr builder e1
          and e2' = expr builder e2 in
	    L.build_call stringcmp [| (e1') ; (e2') |]
	    "stringcmp" builder
      | A.FunExp(f, act) ->
        let (fdef, fdecl) = StringMap.find f func_decls in
        let actuals = List.rev (List.map (expr builder) (List.rev act)) in
        let result = (match fdecl.A.type_spec with A.Null -> ""
                                            | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list actuals) result builder
      in
(*
  | A.ArrAssign  (*TODO*)
  | A.AssignObj  (*TODO*)
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
