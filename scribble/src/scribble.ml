open Base

module Sexp = Sexplib0.Sexp
type sexp = Sexp.t = Atom of string | List of sexp list

module Parser = struct
  open Parsec

  module Token = struct
    let token parser =
      parser            >>= fun value ->
      option whitespace >>= fun _ ->
      return value

    let atom = token (Char.one_or_more letter)
    let left_paren = token (char '(')
    let right_paren = token (char ')')
    let left_bracket = token (char '[')
    let right_bracket = token (char ']')
    let left_brace = token (char '{')
    let right_brace = token (char '}')

    module String = struct
(*       let body = Char.one_or_more letter *)

      let not_escape =
        not_followed_by (Char.set "\"\\") >>= fun () ->
        any

      let newline = string {|\n|} >=> return '\n'
      let tab = string {|\t|} >=> return '\t'
      let carrige_return = string {|\r|} >=> return '\r'

      let escape = newline <|> tab <|> carrige_return

      let body = Char.zero_or_more (not_escape <|> escape)

      let it = token (around (char '"') body (char '"'))
    end

    let string = String.it

  end

  let in_parens parser = around Token.left_paren parser Token.right_paren
(*   let in_brackets parser = around Token.left_bracket parser Token.right_bracket *)
(*   let in_braces parser = around Token.left_brace parser Token.right_brace *)

  module Sexp = struct
    let atom =
      Token.atom >>= fun atom ->
      return (Atom atom)

    let string =
      Token.string >>= fun string ->
      return (Atom string)

    let rec sexp_list x = begin
      zero_or_more sexp >>= fun list ->
      return list
    end x

    and list x = begin
      in_parens sexp_list >>= fun list ->
      return (List list)
    end x

    and sexp x = begin
      atom <|> string <|> list
    end x
  end

  let sexp = Sexp.sexp


  module Scribble = struct
    let command = sexp

    (* Spaces after left bracket are allowed, but not after right bracket *)
    let datum = around Token.left_bracket Sexp.sexp_list (char ']')

    module Text = struct
      let not_escape = not_followed_by (Char.set "@{}") >>= fun () -> any
      let not_escapes = Char.one_or_more not_escape

      let rec body_item x = begin
        not_escapes <|> begin
          char '{' >>= fun _ ->
          body_items >>= fun nested ->
          char '}' >>= fun _ ->
          return ("{" ^ nested ^ "}")
        end
      end x

      and body_items x = begin
        zero_or_more body_item >>= fun list ->
        return (String.concat list)
      end x



(*       let body_item = not_escapes <|> escape *)

      let body =
        zero_or_more body_item >>= fun list ->
        match String.concat list with
        | "" -> return []
        | string -> return [Atom string]


(*
      let escape = char '@'

      let body' =
        Char.zero_or_more (not_escape <|> escape) >>= fun string ->
        match string with
        | "" -> return []
        | string -> return [Atom string]
*)
    end

    let text_body = around (char '{') Text.body (char '}')

    let scribble =
      char '@' >=>
      command >>= fun sexp ->
      option datum >>= fun datum ->
      option text_body >>= fun text_body ->
      match datum, text_body with
      | None, None ->
          return sexp
      | datum, text_body ->
          let unoption = Option.value ~default:[] in
          let datum, text_body = unoption datum, unoption text_body in
          return (List (sexp :: datum @ text_body))

  end

  let scribble = Scribble.scribble

end

