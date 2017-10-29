(* Semantic checking for the Gantry compiler *)

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
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  (* Are two sides of assignment compatible? *)
  let check_assign lvaluet rvaluet err = 
     if lvaluet == rvaluet then lvaluet else raise err
  in

  (**** Checking Global Variables ****)
  (* TODO: Add additional checks for statements, along the lines of 138 in Gantry semant.ml *)
  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

  (**** Checking Functions ****)
  (* TODO : This will need to be modified from micro C because we don't have main, microC checks that main is defined, but I can't see it referenced anywhere else. I think lines 59-67 put the main function at the front of the function_decls list? *)
   
