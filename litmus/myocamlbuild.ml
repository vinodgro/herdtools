open Ocamlbuild_plugin;;
open Command;;

(* Is there a better way to specify source directory? *)
let pp = A"../pp2ml.sh";;

dispatch begin function
  | After_rules ->
      rule "pp: .mly.tpl -> mly"
        ~prods:["%.mly";]
        ~dep:"%.mly.tpl"
      begin fun env _build ->
        Cmd(S[pp; P(env "%.mly.tpl")])
      end ;
      rule "pp: .mll.tpl -> mll"
        ~prods:["%.mll";]
        ~dep:"%.mll.tpl"
      begin fun env _build ->
        Cmd(S[pp; P(env "%.mll.tpl")])
      end ;

  | _ -> ()
end
