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
   A.Bool -> i1_t
   A.Float-> float_t (*Not sure about this https://llvm.org/docs/LangRef.html#floating-point-types*)
   A.Null -> void_t (*LHS refers to the name in our language right?*) in

(* This is currently the same as the one in microC, I don't think it references anything specific to gantry *)
 let global_vars =
  let global_var m (t, n) = 
  let init = L.const_int (ltype_of_typ t) 0
  in StringMap.add n (L.define_global n init the_module) m in 
  List.fold_left global_var StringMap.empty globals
