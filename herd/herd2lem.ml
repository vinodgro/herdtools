open AST
open Printf

let rec fprintf_list s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> fprintf chan "(%s %a %a)" s f x (fprintf_list s f) xs

let rec fprintf_list_infix s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> 
    fprintf chan "(%a %s %a)" 
      f x s (fprintf_list_infix s f) xs

let rec fprintf_list_infix' s s' f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> 
    fprintf chan "(%a %s (%s %a))" 
      f x s s' (fprintf_list_infix' s s' f) xs


let lem_of_konst chan = function
  | Empty SET -> fprintf chan "emps"
  | Empty RLN -> fprintf chan "empr"

let rec lem_of_op2 chan es = function
  | Union _ -> fprintf_list_infix "union" lem_of_exp chan es
  | Inter SET -> fprintf_list_infix "inter" lem_of_exp chan es 
  | Inter RLN -> fprintf_list_infix "inter" lem_of_exp chan es 
  | Diff SET -> fprintf_list_infix' "inter" "comps X" lem_of_exp chan es
  | Diff RLN -> fprintf_list_infix' "inter" "compr X" lem_of_exp chan es
  | Seq -> fprintf_list "seq" lem_of_exp chan es
  | Cartesian -> fprintf_list "cross" lem_of_exp chan es

and lem_of_op1 chan e = function
  | Plus -> fprintf chan "(tc %a)" lem_of_exp e
  | Star -> fprintf chan "(rtc %a)" lem_of_exp e
  | Opt -> fprintf chan "(rc X %a)" lem_of_exp e
  | Select _ -> fprintf chan "Select not done yet"
  | Inv -> fprintf chan "(inv %a)" lem_of_exp e
  | Ext -> fprintf chan "(ext X %a)" lem_of_exp e
  | Int -> fprintf chan "(int X %a)" lem_of_exp e
  | NoId -> fprintf chan "(noid %a)" lem_of_exp e
  | Set_to_rln -> fprintf chan "(stor %a)" lem_of_exp e
  | Comp SET -> fprintf chan "(comps X %a)" lem_of_exp e
  | Comp RLN -> fprintf chan "(compr X %a)" lem_of_exp e

and lem_of_exp chan = function
  | Konst k -> lem_of_konst chan k
  | Var x -> begin match x with
      | "rf" | "asw" | "lo" -> 
        fprintf chan "X.%sh" x
      | "po" | "addr" | "data" | "co" -> 
        fprintf chan "X.%s" x
      | "_" -> fprintf chan "(unis X)"
      | _ -> fprintf chan "(%s X)" x
    end 
  | Op1 (op1, e) -> lem_of_op1 chan e op1
  | Op (op2, es) -> lem_of_op2 chan es op2
  | App _ -> fprintf chan "Local bindings not done yet"
  | Bind _ -> fprintf chan "Local bindings not done yet"
  | BindRec _ -> fprintf chan "Local bindings not done yet"
  | Fun _ -> fprintf chan "Local bindings not done yet"

and lem_of_binding chan (x, e) = 
  fprintf chan "let %s X = %a" x lem_of_exp e

let fprintf_so x chan so = 
  fprintf chan "%s" (match so with
    | None -> x
    | Some s -> s)

let lem_of_test = function
  | Acyclic -> "acyclic"
  | Irreflexive -> "irreflexive"
  | TestEmpty -> "is_empty"

let provides : string list ref = ref []
let requires : string list ref = ref []
let seen_requires_clause : bool ref = ref false

let lem_of_ins chan = function
  | Let bs -> List.iter (lem_of_binding chan) bs
  | Rec bs -> List.iter (lem_of_binding chan) bs 
  (* doesn't handle recursion properly *)
  | Test (_, test, exp, name, test_type) -> 
    let name = begin match name with 
        | None -> Warn.user_error "Unnamed test!" 
        | Some name -> name 
    end in
    fprintf chan "let %s X = %s %a" 
      name
      (lem_of_test test)
      lem_of_exp exp;
    begin match test_type with
      | Provides -> 
        if (!seen_requires_clause) then
          Warn.user_error "Provides-clause follows requires-clause!";
        provides := name :: (!provides)
      | Requires -> 
        seen_requires_clause := true;
        requires := name :: (!requires)
    end
  | UnShow _ -> ()
  | Show _ -> ()
  | ShowAs _ -> ()

let lem_of_prog chan prog = 
  fprintf chan "open import Pervasives\n";
  fprintf chan "open import Relation\n";
  fprintf chan "open import Herd\n\n";
  List.iter (fprintf chan "%a\n\n" lem_of_ins) prog;
  fprintf chan "let provides X =\n";
  fprintf chan "  herd_provides X &&\n";
  List.iter (fprintf chan "  %s X &&\n") (List.rev (!provides));
  fprintf chan "  true\n\n";
  fprintf chan "let requires X =\n";
  List.iter (fprintf chan "  %s X &&\n") (List.rev (!requires));
  fprintf chan "  true\n\n";
  fprintf chan "let model =\n";
  fprintf chan "  <| provides = provides;\n";
  fprintf chan "     requires = requires;\n";
  fprintf chan "  |>\n";
