module L = Llvm
module A = Ast

module StringMap = Map.Make(String)

(* Hash Tables for our variable bindings *)
let g_var_tbl  : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;
let f_var_tbl  : (string, L.llvalue) Hashtbl.t = Hashtbl.create 10;;

(* Stack and Buffer for building Objects *)
let kv_stack = Stack.create ();;
let key_buf  = Buffer.create 10;;

let translate (globals, functions) =

  let context = L.global_context () in
  let the_module = L.create_module context "Gantry"

    (* LLVM Types *)
    and i1_t   = L.i1_type context
    and i32_t  = L.i32_type context
    and i8_t   = L.i8_type context
    and b_t    = L.i8_type context
    and str_t  = L.pointer_type (L.i8_type context)
    and flt_t  = L.double_type context
    and void_t = L.void_type context in
    (* Array Types *)
    let arr_int_t = L.named_struct_type context "arr_int_t" in
                let body =
                  [|
                    i32_t; (* arr length *)
                    i32_t; (* arr type *)
                    L.pointer_type i32_t;
		   |] in
                  ignore (L.struct_set_body arr_int_t body true);
    let arr_flt_t = L.named_struct_type context "arr_flt_t" in
                let body =
                  [|
                    i32_t; (* arr length *)
                    i32_t; (* arr type *)
                    L.pointer_type flt_t;
		   |] in
                  ignore (L.struct_set_body arr_flt_t body true);
    let arr_str_t = L.named_struct_type context "arr_str_t" in
                let body =
                  [|
                    i32_t; (* arr length *)
                    i32_t; (* arr type *)
                    L.pointer_type str_t;
		   |] in
                  ignore (L.struct_set_body arr_str_t body true);
    let arr_bool_t = L.named_struct_type context "arr_bool_t" in
                let body =
                  [|
                    i32_t; (* arr length *)
                    i32_t; (* arr type *)
                    L.pointer_type b_t;
		   |] in
                  ignore (L.struct_set_body arr_bool_t body true);
    (* Object Type *)
    let obj_t = L.named_struct_type context "obj" in
                let body =
                  [|
                    L.pointer_type obj_t; (* next *)
                    str_t; (* key *)
                    (* values *)
                    i32_t; (* value type *)
                    i32_t; (* int *)
                    flt_t; (* float *)
                    L.pointer_type obj_t; (* child (object) *)
                    str_t; (* string *)
                    b_t;  (* bool *)
		    (* Arrays *)
		    L.pointer_type arr_int_t;
		    L.pointer_type arr_flt_t;
		    L.pointer_type arr_str_t;
		    L.pointer_type arr_bool_t;
                  |] in
                  ignore (L.struct_set_body obj_t body true);
    let arr_obj_t = L.named_struct_type context "arr_obj_t" in
                let body =
                  [|
                    i32_t; (* arr length *)
                    L.pointer_type (L.pointer_type obj_t);
                    i32_t; (* arr type *)
		   |] in
                  ignore (L.struct_set_body arr_obj_t body true);

    (* AST to LLVM types *)
    let ltype_of_typ = function
        A.Int      -> i32_t
      | A.Float    -> flt_t
      | A.Object   -> L.pointer_type obj_t
      | A.String   -> str_t
      | A.Bool     -> b_t
      | A.Null     -> void_t
      (* Array Types *)
      | A.Int_Array    -> L.pointer_type arr_int_t
      | A.Float_Array  -> L.pointer_type arr_flt_t
      | A.Object_Array -> L.pointer_type arr_obj_t
      | A.String_Array -> L.pointer_type arr_str_t
      | A.Bool_Array   -> L.pointer_type arr_bool_t
    in

    (* Global Declarations *)
    let add_global (t, n) =
    let init = L.const_null (ltype_of_typ t) in
    Hashtbl.add g_var_tbl n (L.define_global n init the_module) in
    ignore(List.iter add_global globals);

    (* Printf Built-in *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* Print bool *)
    let printb_t = L.var_arg_function_type i32_t [| b_t |] in
    let printb_func = L.declare_function "print_b" printb_t the_module in

    (* String Concatenation *)
    let string_concat_t = L.var_arg_function_type str_t [| L.pointer_type i8_t |] in
    let string_concat = L.declare_function "string_concat" string_concat_t the_module in

    (* String Slice *)
    let slice_t = L.var_arg_function_type str_t [| L.pointer_type i8_t ; i32_t ; i32_t |] in
    let slice = L.declare_function "slice" slice_t the_module in
    
    (* String Comparison *)
    let stringcmp_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
    let stringcmp = L.declare_function "stringcmp" stringcmp_t the_module in
    
    (* String Equal *)
    let stringeq_t = L.var_arg_function_type i8_t
		     [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
    let stringeq = L.declare_function "stringeq" stringeq_t the_module in
   
    (* String Length *)
    let string_length_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let string_length = L.declare_function "string_length" string_length_t the_module in

    (* Array Length *)
    let arr_length_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let arr_length = L.declare_function "arr_length" arr_length_t the_module in

    (* HTTP GET library function *)
    let httpget_t = L.var_arg_function_type str_t [| L.pointer_type i8_t |] in
    let httpget_func = L.declare_function "httpget" httpget_t the_module in

    (* HTTP POST library function *)
    let httppost_t = L.var_arg_function_type str_t
		     [| L.pointer_type i8_t ; L.pointer_type i8_t |] in
    let httppost_func = L.declare_function "httppost" httppost_t the_module in

    (* Nap (sleep wrapper) library function *)
    let nap_t = L.var_arg_function_type i32_t [| i32_t |] in
    let nap_func = L.declare_function "nap" nap_t the_module in
    
    (* Array Stringify *)
    let arr_stringify_t = L.var_arg_function_type (L.pointer_type i8_t)
			[| L.pointer_type i8_t |] in
    let arr_stringify = L.declare_function "arr_stringify"
			arr_stringify_t the_module in

    (* Object Library Runtime *)
    let obj_stringify_t = L.var_arg_function_type (L.pointer_type i8_t)
			[| L.pointer_type obj_t |] in
    let obj_stringify = L.declare_function "obj_stringify"
			obj_stringify_t the_module in
    
    let obj_addkey_t = L.var_arg_function_type (L.pointer_type obj_t)
			[| L.pointer_type obj_t ; (L.pointer_type i8_t) ; i32_t; (L.pointer_type i8_t)  |] in
    let obj_addkey = L.declare_function "obj_addkey"
			obj_addkey_t the_module in
    
    let obj_findkey_t = L.var_arg_function_type (L.pointer_type obj_t)
			[| L.pointer_type obj_t ; L.pointer_type i8_t |] in
    let obj_findkey_func = L.declare_function "obj_findkey"
			obj_findkey_t the_module in

    let obj_getkey_t = L.var_arg_function_type (L.pointer_type i8_t)
			[| L.pointer_type obj_t |] in
    let obj_getkey_func = L.declare_function "obj_getkey"
			obj_getkey_t the_module in

    let obj_assign_t =
		L.var_arg_function_type i32_t
		[| L.pointer_type obj_t ; i32_t ; (L.pointer_type i8_t) |] in
    let obj_assign_func = L.declare_function "obj_assign" obj_assign_t the_module in

    let obj_gettyp_t = L.var_arg_function_type i32_t [| L.pointer_type obj_t |] in
    let obj_gettyp_func = L.declare_function "obj_gettyp" obj_gettyp_t the_module in

    let printk_t = L.var_arg_function_type i32_t [| L.pointer_type obj_t |] in
    let printk_func = L.declare_function "print_k" printk_t the_module in

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

      (* Variable Lookup *)
      let lookup n =
        (* Local Lookup *)
        try Hashtbl.find f_var_tbl n with
        Not_found -> 
        (* Global Lookup *)
	try Hashtbl.find g_var_tbl n with
	Not_found ->
	(* Object Key Lookup *)
	let l = Str.split (Str.regexp_string ".") n in
	  (* Get 'base' object to search *)
	  let obj = List.hd l in
	  (* Get object ptr from locals *)
	  let obj_p =
            try Hashtbl.find f_var_tbl obj with
            Not_found -> Hashtbl.find g_var_tbl obj
	  in
	  (* Build runtime call to find key *)
	  let keys = String.concat "." (List.tl l) in
	  let key = L.build_global_stringptr keys "keys_find" builder in
	  (* Dereference parent object pointer *)
	  let obj_p = L.build_load obj_p "tmp" builder in
	  (* Now build runtime call to find the key *)
	  L.build_call obj_findkey_func [| obj_p ; key |]
	  "obj_findkey" builder
      in

    (* Construct code for an expression and return the value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float flt_t f
      | A.StrLit s -> L.build_global_stringptr s "string" builder
      | A.BoolLit b -> L.const_int b_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> if (String.contains s '.') then
		    (lookup s)
		  else
		    (L.build_load (lookup s) s builder)
      | A.Binop (e1, op, e2) ->
        let e1' = expr builder e1
        and e2' = expr builder e2 in
	let typ = L.string_of_lltype (L.type_of e1') in
	(match typ with 
           "i32" | "i8"  -> (match op with
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
	   | _      -> raise (Failure ("Invalid integer binary operation"))
	   ) e1' e2' "tmp" builder
         | "double" -> (match op with
             A.Add  -> L.build_fadd
           | A.Sub  -> L.build_fsub
           | A.Mult -> L.build_fmul
           | A.Div  -> L.build_fdiv
           | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
           | A.Neq  -> L.build_fcmp L.Fcmp.One
           | A.Lt   -> L.build_fcmp L.Fcmp.Ult
           | A.Leq  -> L.build_fcmp L.Fcmp.Ole
           | A.Gt   -> L.build_fcmp L.Fcmp.Ogt
           | A.Geq  -> L.build_fcmp L.Fcmp.Oge
	   | _      -> raise (Failure ("Invalid float binary operation"))
           ) e1' e2' "tmp" builder
        | "i8*" -> (match op with
            A.Eq    ->  L.build_icmp L.Icmp.Eq
                       (L.build_call stringeq [| (e1') ; (e2') |] "stringeq" builder)
                       (L.const_int b_t 1) "tmp" builder

          | A.Neq   ->  L.build_icmp L.Icmp.Eq
                       (L.build_call stringeq [| (e1') ; (e2') |] "stringeq" builder)
                       (L.const_int b_t 0) "tmp" builder

          | A.Conc  -> L.build_call string_concat [| e1'; e2'|] "string_concat" builder
	  | _       -> raise (Failure ("Invalid string binary operation")))
	| _         -> raise (Failure ("Invalid binary operation"))
       )
      | A.Unop(op, e) ->
        let e' = expr builder e in
        (match op with
             A.Neg  -> L.build_neg e' "tmp" builder
           | A.Not  -> L.build_intcast (L.build_not
				       (L.build_intcast e' i1_t "tmp" builder)
				       "tmp" builder) i8_t "tmp" builder
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
      | A.ArrExp(el) ->
	let vl  = List.map (expr builder) el in
	(* We infer type by first expression - semantic should handle this *)
	let typ  = L.pointer_type (L.type_of (List.hd vl)) in
	let size = L.const_int i32_t (List.length vl) in
	let arr_typ = (match L.string_of_lltype typ with
			  "i32*"    -> arr_int_t
			| "double*" -> arr_flt_t
			| "%obj**"  -> arr_obj_t
			| "i8**"    -> arr_str_t
			| "i8*"     -> arr_bool_t
			| _         -> raise (Failure ("Invalid array type"))) in
	(* Declare Array *)
	let arr  = L.build_array_malloc typ size "arr" builder in
	let arr  = L.build_pointercast arr typ "arr" builder in
	(* Declare Struct *)
	let arr_struct = L.build_malloc arr_typ "arr_struct" builder in
	(* Set array size field *)
	let arr_l = L.build_struct_gep arr_struct 0 "arr_size" builder in
	ignore(L.build_store (L.const_int i32_t
			     (List.length vl)) arr_l builder);
	(* Set Array Type in Struct *)
	let arr_t = L.build_struct_gep arr_struct 1 "arr_type" builder in
	let sidx_of_typ = function
	    "i32*"    -> 3
	  | "double*" -> 4
	  | "%obj**"  -> 5
	  | "i8**"    -> 6
	  | "i8*"     -> 7
	  | _         -> raise (Failure ("Invalid array type"))
	in
	let t = sidx_of_typ (L.string_of_lltype typ) in
	ignore(L.build_store (L.const_int i32_t t) arr_t builder);
	(* For each value, store in array *)
	let fill i v =
	  let vp =
	  L.build_gep arr [| L.const_int i32_t (i + 1) |] "arr_v" builder in
	  ignore(L.build_store v vp builder);
	in
	(* Populate Array with Values *)
	List.iteri fill vl;
	(* Store Array in Struct *)
	let arr_p = L.build_struct_gep arr_struct 2 "arr_struct" builder in
	ignore(L.build_store arr arr_p builder);
	arr_struct
      | A.ArrAcc(e1, e2) ->
        let e1_str = A.expr_to_str e1
        and idx = expr builder e2 in
	let idx = L.build_add idx (L.const_int i32_t 2) "arridx" builder in
	let arr =
          if (String.contains e1_str '.') then
	    (lookup e1_str)
	  else
	    (L.build_load (lookup e1_str) "arracc" builder)
	in
	(* Get actual array from struct *)
	let arr = L.build_struct_gep arr 2 "arr_v" builder in
	let arr = L.build_load arr "arr_v" builder in
	let e1' = L.build_gep arr [| idx |] "arracc_e" builder in
	let e1' = L.build_load e1' "arracc" builder in
	e1'
      | A.KeyVal(t, n, e) ->
	(* Resolve struct index and 'value_typ' in struct *)
	let sidx_of_typ = function
	    A.Int    -> 3 
	  | A.Float  -> 4
	  | A.Object -> 5
	  | A.String -> 6
	  | A.Bool   -> 7
	  (* Arrays *)
	  | A.Int_Array    -> 8
	  | A.Float_Array  -> 9
	  | A.String_Array -> 10
	  | A.Bool_Array   -> 11
	  | _ -> raise (Failure ("Invalid or unsupported object key type"))
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
	ignore(Stack.push (sidx_of_typ t, n, k) kv_stack);
        e'
      | A.ObjExp(el) ->
	let null = (L.const_pointer_null (L.pointer_type obj_t)) in
	ignore(Stack.push (-1, "NULL", null) kv_stack);
	(* Set next for a key or object *)
	let set_next k =
	try
	  let (t, _, next_k) = Stack.top kv_stack in
	  if t > -1 then 
	    (ignore(L.build_store next_k
	           (L.build_struct_gep k 0 "next" builder) builder);)
	  else
	    (ignore(L.build_store
		   (L.const_pointer_null (L.pointer_type obj_t))
	           (L.build_struct_gep k 0 "next" builder) builder);
		   ignore(Stack.pop kv_stack);)
	with
	  Stack.Empty -> ()
	in
	(* Resolve the list of key value exprs (putting them into a stack) *)
	let kv = List.map (expr builder) el in
	(* The Enclosing (parent) Object *)
	let parent = L.build_malloc obj_t "obj" builder in
	(* Connect parent to first key *)
	ignore(set_next parent);
	(* Connect each key in this object *)
	let build_obj _ =
	  let (t, _, k) = Stack.pop kv_stack in
	  ignore(L.build_struct_gep k t "value" builder);
	  ignore(set_next k);
	in
	(* Build out all of the key values within this object *)
	List.iter build_obj kv;
	(* Return the pointer to the enclosing object *)
	parent
      | A.AssignDecl(t, n, e) ->
        (* First add this declaration to f_var_tbl hash map *)
        ignore (add_local (t, n) builder);
	(* Non-Object on RHS *)
	if (not (String.contains (A.expr_to_str e) '.')) then (
          let e' = expr builder e in
	  let n = lookup n in
          ignore (L.build_store e' n builder);
          e'
	)
	(* Object on RHS *)
	else (
	  let e1_str = n
	  and e2_str = A.expr_to_str e in
	  let e1' = lookup e1_str
	  and e2' = lookup e2_str in
	  let sidx_of_typ = function
	      "i32*"    -> 3
	    | "double*" -> 4
	    | "%obj**"  -> 5
	    | "i8**"    -> 6
	    | "i8*"     -> 7
	    (* Arrays *)
	    | "%arr_int_t**"  -> 8
	    | "%arr_flt_t**"  -> 9
	    | "%arr_str_t**"  -> 10
	    | "%arr_bool_t**" -> 11
	    | _         -> raise (Failure ("Invalid object assignment"))
	  in
	  (* Get type of primitive on LHS *)
	  let t = sidx_of_typ (L.string_of_lltype (L.type_of e1')) in
	  let t_e1' = L.const_int i32_t t in
	  (* Get Value of Object on RHS (based on e1' type) *)
	  let v_e2' = L.build_call obj_getkey_func
		      [| e2' ; t_e1' |] "obj_getkey" builder in
	  (* Cast void* RHV to ptr of LHV type *)
	  ignore(L.build_alloca (L.type_of e1') "pcst" builder);
	  let v_e2' = L.build_bitcast v_e2' (L.type_of e1') "cst" builder in
	  let v_e2' = L.build_load v_e2' "loadcst" builder in
	  ignore(L.build_store v_e2' e1' builder);
	  v_e2'
	)
      | A.Assign(e1, e2) ->
	(* Object Assignment *)
	let e1_str = A.expr_to_str e1
	and e2_str = A.expr_to_str e2 in
	(* Object LHV and RHV: enforce type of RHV *)
	if ((String.contains e1_str '.') && (String.contains e2_str '.')) then (
	  (* Get Objects on LHS and RHS *)
	  let e1' = lookup e1_str
	  and e2' = lookup e2_str in
	  (* Get type of Object on RHS *)
	  let t_e2' = L.build_call obj_gettyp_func
		      [| e2' |] "obj_gettyp" builder in
	  (* Get Value of Object on RHS *)
	  let v_e2' = L.build_call obj_getkey_func
		      [| e2' ; t_e2' |] "obj_getkey" builder in
	  (* Store that value in Object on LHS *)
	  ignore (L.build_call obj_assign_func
	  [| e1' ; t_e2' ; v_e2' |] "obj_assign" builder);
	  e2'
	)
	(* Object LHV Only: enforce type of RHV *)
	else if (String.contains e1_str '.') then (
	  let e1' = lookup e1_str
	  and e2' = expr builder e2 in
	  let sidx_of_typ = function
	      "i32"    -> 3
	    | "double" -> 4
	    | "%obj*"  -> 5
	    | "i8*"    -> 6
	    | "i8"     -> 7
	    (* Arrays *)
	    | "%arr_int_t*"  -> 8
	    | "%arr_flt_t*"  -> 9
	    | "%arr_str_t*"  -> 10
	    | "%arr_bool_t*" -> 11
	    | _        -> raise (Failure ("Invalid object assignment"))
	  in
	  (* Get type of primitive on RHS *)
	  let t = sidx_of_typ (L.string_of_lltype (L.type_of e2')) in
	  let t_e2' = L.const_int i32_t t in
	  (* Cast ptr to RHV to void ptr to store in Object *)
	  let p_e2' = L.build_alloca (L.type_of e2') "pcst" builder in
	  ignore(L.build_store e2' p_e2' builder);
	  let v_e2' = L.build_bitcast p_e2' (L.pointer_type i8_t)
		      "cst" builder in
	  (* Store value from primitive on RHS in Object on LHS *)
	  ignore (L.build_call obj_assign_func
	  [| e1' ; t_e2' ; v_e2' |] "obj_assign" builder);
	  e2'
	)
	(* Object RHV Only: try type of RHV (runtime error on failure) *)
	else if (String.contains e2_str '.') then (
	  let e1' = lookup e1_str
	  and e2' = lookup e2_str in
	  let sidx_of_typ = function
	      "i32*"    -> 3
	    | "double*" -> 4
	    | "%obj**"  -> 5
	    | "i8**"    -> 6
	    | "i8*"     -> 7
	    (* Arrays *)
	    | "%arr_int_t**"  -> 8
	    | "%arr_flt_t**"  -> 9
	    | "%arr_str_t**"  -> 10
	    | "%arr_bool_t**" -> 11
	    | _         -> raise (Failure ("Invalid object assignment"))
	  in
	  (* Get type of primitive on LHS *)
	  let t = sidx_of_typ (L.string_of_lltype (L.type_of e1')) in
	  let t_e1' = L.const_int i32_t t in
	  (* Get Value of Object on RHS (based on e1' type) *)
	  let v_e2' = L.build_call obj_getkey_func
		      [| e2' ; t_e1' |] "obj_getkey" builder in
	  (* Cast void* RHV to ptr of LHV type *)
	  ignore(L.build_alloca (L.type_of e1') "pcst" builder);
	  let v_e2' = L.build_bitcast v_e2' (L.type_of e1') "cst" builder in
	  let v_e2' = L.build_load v_e2' "loadcst" builder in
	  ignore(L.build_store v_e2' e1' builder);
	  v_e2'
	)
	(* Primitive Assignment *)
	else (
	  let e2' = expr builder e2 in
	  let e1' = lookup e1_str in
	  ignore (L.build_store e2' e1' builder);
	  e2'
	)
      | A.FunExp("print_i", [e]) ->
	L.build_call printf_func [| int_format_str ; (expr builder e) |]
	  "print_i" builder
      | A.FunExp("print_s", [e]) ->
	L.build_call printf_func [| str_format_str ; (expr builder e) |]
	  "print_s" builder
      | A.FunExp("print_d", [e]) ->
	L.build_call printf_func [| flt_format_str ; (expr builder e) |]
	  "print_d" builder
      | A.FunExp("print_b", [e]) ->
        L.build_call printb_func [| (expr builder e) |] 
          "print_b" builder       
      | A.FunExp("print_k", [e]) ->
	L.build_call printk_func [| (expr builder e) |]
	  "print_k" builder
      | A.FunExp("httpget", [e]) ->
        L.build_call httpget_func [| (expr builder e) |]
          "httpget" builder
      | A.FunExp("httppost", [e; e2]) ->
          let e2' = expr builder e2 in
            L.build_call httppost_func [| (expr builder e) ; (e2') |]
            "httppost" builder
      | A.FunExp("nap", [e]) ->
            L.build_call nap_func [| (expr builder e) |]
            "nap" builder
      | A.FunExp("string_length", [e]) ->
	    L.build_call string_length [| (expr builder e ) |]
	    "string_length" builder
      | A.FunExp("arr_length", [e]) ->
	    let e' = expr builder e in
	    (* Cast array to void * to send to length function *)
	    let p_e' = L.build_alloca (L.type_of e') "pcst" builder in
	    ignore(L.build_store e' p_e' builder);
	    let e' = L.build_bitcast p_e' (L.pointer_type i8_t) "cs" builder in
	    L.build_call arr_length [| (e') |] "arr_length" builder
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
      | A.FunExp("arr_stringify", [e]) ->
	  let e' = expr builder e in
	  (* Cast e to void ptr  *)
	  let p_e' = L.build_alloca (L.type_of e') "pcst" builder in
	  ignore(L.build_store e' p_e' builder);
	  let v_e' = L.build_bitcast p_e' (L.pointer_type i8_t)
		      "cst" builder in
	  (* Build call to obj_addkey *)
	  L.build_call arr_stringify [| (v_e') |]
	    "arr_stringify" builder
      | A.FunExp("obj_stringify", [e]) ->
	  let e' = expr builder e in
	    L.build_call obj_stringify [|(e')|]
	    "obj_stringify" builder
      | A.FunExp("obj_addkey", [e; e1; e2; e3]) ->
	  let e' = expr builder e
	  and e1' = expr builder e1
	  and e2' = expr builder e2
	  and e3' = expr builder e3 in 
	  (* Cast e3 to void ptr to store in Object *)
	  let p_e3' = L.build_alloca (L.type_of e3') "pcst" builder in
	  ignore(L.build_store e3' p_e3' builder);
	  let v_e3' = L.build_bitcast p_e3' (L.pointer_type i8_t)
		      "cst" builder in
	  (* Build call to obj_addkey *)
	  L.build_call obj_addkey [| (e'); (e1'); (e2'); (v_e3') |]
	    "obj_addkey" builder
      | A.FunExp(f, act) ->
        let (fdef, fdecl) = 
	StringMap.find f func_decls in
	(* If Object is actual, cast its value to formal's type *)
	let resolve_acts form_t act =
	if (String.contains (A.expr_to_str act) '.') then (
	  let e' = lookup (A.expr_to_str act) in
	  let sidx_of_typ = function
	      "int"    -> 3
	    | "float"  -> 4
	    | "object" -> 5
	    | "string" -> 6
	    | "bool"   -> 7
	    (* Arrays *)
	    | "%arr_int_t"  -> 8
	    | "%arr_flt_t"  -> 9
	    | "%arr_str_t"  -> 10
	    | "%arr_bool_t" -> 11
	    | _        -> raise (Failure ("Invalid object function actual parameter"))
	  in
	  (* Get type of formal *)
	  let t = sidx_of_typ (A.string_of_typ form_t) in
	  let t = L.const_int i32_t t in
	  (* Get Value of Object on RHS (based on formal type) *)
	  let v_e' = L.build_call obj_getkey_func [| e' ; t |] "obj_getkey_param" builder in
	  (* Cast void* RHV to ptr of LHV type *)
	  let v_e' = L.build_bitcast v_e' (L.pointer_type (ltype_of_typ form_t)) "cst" builder in
	  let v_e' = L.build_load v_e' "loadcst" builder in
	  v_e'
	)
	else (
	  expr builder act
	) in
	let formals = List.map (fun (t, _) -> t) fdecl.A.f_params in
        let actuals = List.rev (List.map2 (resolve_acts) (List.rev formals) (List.rev act)) in
        let result = (match fdecl.A.type_spec with
		        A.Null -> ""
                      | _      -> f ^ "_result") in
        L.build_call fdef (Array.of_list actuals) result builder
      in


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
      | A.If (predicate, then_stmt, else_stmt) ->
        let bool_val = expr builder predicate in
	let typ = L.string_of_lltype (L.type_of bool_val) in
	let bv = 
	if  ((String.compare typ "i8") == 0) then
		(L.build_intcast bool_val i1_t "tmp" builder)
	else
		bool_val in

        let merge_bb = L.append_block context "merge" the_function in

        let then_bb = L.append_block context "then" the_function in
        add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
        (L.build_br merge_bb);

        let else_bb = L.append_block context "else" the_function in
        add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
        (L.build_br merge_bb);

        ignore (L.build_cond_br bv then_bb else_bb builder);
        L.builder_at_end context merge_bb
      | A.While (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        ignore (L.build_br pred_bb builder);

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (stmt (L.builder_at_end context body_bb) body)
        (L.build_br pred_bb);

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr pred_builder predicate in
	let typ = L.string_of_lltype (L.type_of bool_val) in
	let bv = 
	if  ((String.compare typ "i8") == 0) then
		(L.build_intcast bool_val i1_t "tmp" builder)
	else
		bool_val in


        let merge_bb = L.append_block context "merge" the_function in
        ignore (L.build_cond_br bv body_bb merge_bb pred_builder);
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
