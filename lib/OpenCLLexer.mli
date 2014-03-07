(*copied from the ppc lexer*)

module Make : functor(O:LexUtils.Config) -> sig
  val token : Lexing.lexbuf -> OpenCLParser.token
end

