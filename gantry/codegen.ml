module L = Llvm
module A = Ast

module StringMap = Map.Make(String)


let translate (globals, functions) = 
 let context = L.global_context () in
 let the_module = L.create_module context "Gantry"
(*Not sure if we need the same types http://llvm.org/doxygen/MachineValueType_8h_source.html - i32 i8 i1*)
 and i32_t = L.i32_type  context
 and i8_t = L.i8_type  context
 and i1_t = L.i1_type  context
 and void_t = L.void_type context in
 
 let ltype_of_typ = function
   A.Int  -> i32_t
   | A.Bool -> i1_t
   | A.Float-> float_t (*Not sure about this https://llvm.org/docs/LangRef.html#floating-point-types*)
   | A.Null -> void_t (*LHS refers to the name in our language right?*) in

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


