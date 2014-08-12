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
    fprintf chan "%a %s %a" 
      f x s (fprintf_list_infix s f) xs

let rec list_iter_alt f inbetween = function
  | [] -> ()
  | [x] -> f x
  | x :: xs  -> f x ; inbetween () ; list_iter_alt f inbetween xs

let tex_of_konst chan = function
  | Empty SET -> fprintf chan "\\{\\}"
  | Empty RLN -> fprintf chan "\\emptyset"

let rec tex_of_op2 chan es = function
  | Union -> fprintf_list_infix "\\cup" tex_of_exp chan es
  | Inter -> fprintf_list_infix "\\cap" tex_of_exp chan es
  | Diff -> fprintf_list_infix "\\setminus" tex_of_exp chan es
  | Seq -> fprintf_list ";" tex_of_exp chan es
  | Cartesian -> fprintf_list "\\times" tex_of_exp chan es

and string_of_dir = function
  | Write -> "W" 
  | Read -> "R"
  | WriteRead -> "M"
  | Atomic -> "A"
  | Plain -> "P"
  | Unv_Set -> "\\_"
  | Bar_Set -> "B"

and tex_of_op1 chan e = function
  | Plus -> fprintf chan "(%a^+)" tex_of_exp e
  | Star -> fprintf chan "(%a^*)" tex_of_exp e
  | Opt -> fprintf chan "(%a^?)" tex_of_exp e
  | Select (d1,d2) -> 
    fprintf chan "\\mathrm{%s%s}(%a)"
      (string_of_dir d1)
      (string_of_dir d2)
      tex_of_exp e
  | Inv -> fprintf chan "(%a^{-1})" tex_of_exp e
  | Square -> fprintf chan "(%a^{2})" tex_of_exp e
  | Ext -> fprintf chan "(\\mathrm{ext} %a)" tex_of_exp e
  | Int -> fprintf chan "(\\mathrm{int} %a)" tex_of_exp e
  | NoId -> fprintf chan "(\\mathrm{noid} %a)" tex_of_exp e
  | Set_to_rln -> fprintf chan "[%a]" tex_of_exp e
  | Comp SET -> fprintf chan "(%a^\\mathsf{c})" tex_of_exp e
  | Comp RLN -> fprintf chan "(%a^\\mathsf{c})" tex_of_exp e

and tex_of_exp chan = function
  | Konst k -> tex_of_konst chan k
  | Var x -> tex_of_var chan x
  | Op1 (op1, e) -> tex_of_op1 chan e op1
  | Op (op2, es) -> fprintf chan "("; tex_of_op2 chan es op2; fprintf chan ")"
  | App (e,es) -> 
    fprintf chan "%a(" tex_of_exp e; 
    list_iter_alt (tex_of_exp chan) (fun () -> fprintf chan ",") es;
    fprintf chan ")"
  | Bind _ -> fprintf chan "\\mbox{\\color{red}[Local bindings not done yet]}"
  | BindRec _ -> fprintf chan "\\mbox{\\color{red}[Local bindings not done yet]}"
  | Fun (xs,e) -> 
    fprintf chan "\\lambda %a \\ldotp %a" 
      tex_of_formals xs
      tex_of_exp e

and tex_of_formals chan = function
  | [] -> fprintf chan "()"
  | [x] -> tex_of_var chan x
  | xs -> 
    fprintf chan "("; 
    list_iter_alt (tex_of_var chan) (fun () -> fprintf chan ",") xs ;
    fprintf chan ")";

and tex_of_var chan x =
    let x = Str.global_replace (Str.regexp_string "_") "_{" x in
    let x = if String.contains x '_' then x ^ "}" else x in
    let x = Str.global_replace (Str.regexp_string "-") "\\mbox{-}" x in
    fprintf chan "\\mathit{%s}" x

and tex_of_name chan x =
    let x = Str.global_replace (Str.regexp_string "_") "_{" x in
    let x = if String.contains x '_' then x ^ "}" else x in
    let x = Str.global_replace (Str.regexp_string "-") "\\mbox{-}" x in
    fprintf chan "\\mathrm{%s}" x

and tex_of_binding chan (x, e) = 
  begin match e with
  | Fun (xs,e) ->
    fprintf chan "$%a(%a) = %a$" 
      tex_of_var x 
      tex_of_formals xs
      tex_of_exp e
  | _ ->
    fprintf chan "$%a = %a$" 
      tex_of_var x 
      tex_of_exp e
  end

let tex_of_test = function
  | Acyclic -> "\\kwd{acyclic}"
  | Irreflexive -> "\\kwd{irreflexive}"
  | TestEmpty -> "\\kwd{empty}"

let tex_of_test_type = function
  | Provides -> ""
  | Requires -> "\\KWD{undefined\\_unless}~"

let tex_of_ins chan = function
  | Let bs -> 
    fprintf chan "\\KWD{let}~"; 
    list_iter_alt (tex_of_binding chan) (fun () -> fprintf chan "~\\KWD{and}~") bs
  | Rec bs -> 
    fprintf chan "\\KWD{let}~\\KWD{rec}~"; 
    list_iter_alt (tex_of_binding chan) (fun () -> fprintf chan "~\\KWD{and}~") bs 
  | Test (_, test, exp, name, test_type) -> 
    fprintf chan "%s%s~$%a$"
      (tex_of_test_type test_type)
      (tex_of_test test)
      tex_of_exp exp;
    begin match name with 
    | None -> () 
    | Some name -> fprintf chan "~\\kwd{as}~$\\mathrm{%a}$" tex_of_name name end;
  | UnShow xs ->
    fprintf chan "\\KWD{unshow}~$";
    list_iter_alt (tex_of_var chan) (fun () -> fprintf chan ",") xs;
    fprintf chan "$"
  | Show xs -> 
    fprintf chan "\\KWD{show}~$";
    list_iter_alt (tex_of_var chan) (fun () -> fprintf chan ",") xs;
    fprintf chan "$"
  | ShowAs (exp,name) -> 
    fprintf chan "\\KWD{show}~$%a$~\\kwd{as}~$\\mathrm{%a}$" 
      tex_of_exp exp 
      tex_of_name name
  | Latex s -> 
    fprintf chan "\\end{quote}\n";
    fprintf chan "\\noindent %s" s;
    fprintf chan "\\begin{quote}\n"

let tex_of_prog chan prog = 
  fprintf chan "\\documentclass[12pt]{article}\n";
  fprintf chan "\\usepackage[margin=1in]{geometry}\n";
  fprintf chan "\\usepackage[usenames,dvipsnames]{color}\n";
  fprintf chan "\\newcommand\\KWD[1]{{\\color{RoyalPurple}\\bfseries #1}}\n";
  fprintf chan "\\newcommand\\kwd[1]{{\\color{ForestGreen}#1}}\n";
  fprintf chan "\\begin{document}\n";
  fprintf chan "\\begin{quote}\n";
  List.iter (fprintf chan "\\noindent %a\n\n" tex_of_ins) prog;
  fprintf chan "\\end{quote}\n";
  fprintf chan "\\end{document}\n"
