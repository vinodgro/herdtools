open AST
open Printf

let rec fprintf_list s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> fprintf chan "%s (%a) (%a)" s f x (fprintf_list s f) xs

let rec fprintf_list_infix s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "(%a)" f x
  | x::xs -> fprintf chan "(%a) %s %a" f x s (fprintf_list_infix s f) xs


let lem_of_konst chan = function
  | Empty_set -> fprintf chan "emps"
  | Empty_rel -> fprintf chan "empr"

let rec lem_of_op2 chan es = function
  | Union -> fprintf_list_infix "union" lem_of_exp chan es
  | Inter -> fprintf_list_infix "inter" lem_of_exp chan es 
  | Diff -> fprintf_list "diff" lem_of_exp chan es
  | Seq -> fprintf_list "seq" lem_of_exp chan es
  | Cartesian -> fprintf_list "cross" lem_of_exp chan es

and lem_of_op1 chan e = function
  | Plus -> fprintf chan "tc(%a)" lem_of_exp e
  | Star -> fprintf chan "rtc(%a)" lem_of_exp e
  | Opt -> fprintf chan "rc(%a)" lem_of_exp e
  | Select _ -> fprintf chan "Select not done yet"
  | Inv -> fprintf chan "inv(%a)" lem_of_exp e
  | Ext -> fprintf chan "ext(%a)" lem_of_exp e
  | Int -> fprintf chan "int(%a)" lem_of_exp e
  | NoId -> fprintf chan "noid(%a)" lem_of_exp e

and lem_of_exp chan = function
  | Konst k -> lem_of_konst chan k
  | Var x -> fprintf chan "%s" x
  | Op1 (op1, e) -> lem_of_op1 chan e op1
  | Op (op2, es) -> lem_of_op2 chan es op2
  | App _ -> fprintf chan "Local bindings not done yet"
  | Bind _ -> fprintf chan "Local bindings not done yet"
  | BindRec _ -> fprintf chan "Local bindings not done yet"
  | Fun _ -> fprintf chan "Local bindings not done yet"

and lem_of_binding chan (x, e) = 
  fprintf chan "let %s = %a" x lem_of_exp e

let lem_of_ins chan = function
  | Let bs -> List.iter (lem_of_binding chan) bs
  | Rec bs -> List.iter (lem_of_binding chan) bs 
  (* doesn't handle recursion properly *)
  | Test _ -> ()
  | UnShow _ -> ()
  | Show _ -> ()
  | ShowAs _ -> ()

let lem_of_prog chan = List.iter (fprintf chan "%a\n" lem_of_ins)
