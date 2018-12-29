let (=>) left right = print_string (if left = right then "." else "F")

let source = Parsec.Source.of_string

module Sexp = Sexplib0.Sexp

type sexp = Sexp.t = Atom of string | List of sexp list

let sexp s = Parsec.parse Scribble.Parser.sexp (source s)
let scribble s = Parsec.parse Scribble.Parser.scribble (source s)

module Test_sexp_parsing = struct

  sexp "foo" => Some (Atom "foo");

  sexp "()" => Some (List []);
  sexp "(foo)" => Some (List [Atom "foo"]);
  sexp "(foo bar)" => Some (List [Atom "foo"; Atom "bar"]);
  sexp "(foo bar baz)" => Some (List [Atom "foo"; Atom "bar"; Atom "baz"]);
  sexp "( \r foo \t bar \n )" => Some (List [Atom "foo"; Atom "bar"]);
  sexp "(() bar)" => Some (List [List []; Atom "bar"]);
  sexp "((foo) bar)" => Some (List [List [Atom "foo"]; Atom "bar"]);

  sexp {|"foo"|} => Some (Atom "foo");
  sexp {|"foo bar"|} => Some (Atom "foo bar");
  sexp {|"foo\nbar"|} => Some (Atom "foo\nbar");
  sexp {|"foo
bar"|} => Some (Atom "foo\nbar");
  sexp {|"\tfoo\nbar\r"|} => Some (Atom "\tfoo\nbar\r");
  sexp {|("foo")|} => Some (List [Atom "foo"]);

  sexp {|("foo")|} => sexp "(foo)";

end

module Test_scribble_at_expression_parsing = struct

  scribble {|@foo|} => sexp "foo";
  scribble {|@(foo)|} => sexp "(foo)";
  scribble {|@(foo bar)|} => sexp "(foo bar)";
  scribble {|@"foo"|} => sexp "foo";

  scribble {|@foo[]|} => sexp "(foo)";
  scribble {|@(foo bar)[]|} => sexp "((foo bar))";
  scribble {|@foo{}|} => sexp "(foo)";
  scribble {|@(foo bar){}|} => sexp "((foo bar))";
  scribble {|@foo[]{}|} => sexp "(foo)";
  scribble {|@(foo bar)[]{}|} => sexp "((foo bar))";

  (* TODO this is invalid: spaces here are not allowed *)
  scribble {|@foo     [bar]|} => sexp "(foo bar)";
  scribble {|@"foo"   [bar]|} => sexp "(foo bar)";
  scribble {|@(foo)   [bar]|} => sexp "((foo) bar)";

  scribble {|@foo[ bar]|} => sexp "(foo bar)";
  scribble {|@foo[bar (baz)]|} => sexp "(foo bar (baz))";

  scribble {|@foo{bar baz}|} => sexp {|(foo "bar baz")|};

  scribble {|@foo{   }|} => sexp {|(foo "   ")|};

  scribble {|@foo[bar]{baz}|} => sexp "(foo bar baz)";

  scribble {|@foo{ { } }|} => sexp {|(foo " { } ")|};
  scribble {|@foo{ { { } } }|} => sexp {|(foo " { { } } ")|};
  scribble {|@foo{{{}}}|} => sexp {|(foo "{{}}")|};

  scribble {|@foo{a @baz z}|} => sexp {|(foo "a " bar " z")|};

(*   scribble "@foo{bar baz}" => sexp {|(foo "bar baz"|}; *)

end




(*
let () = [%sexp ("foo", "bar") ] => List [Atom "foo"; Atom "bar"]
let () = [%sexp "foo", "bar" ] => List [Atom "foo"; Atom "bar"]
*)

(*
let () = ()
  ; scribble "@foo{blah blah blah}" => {|(foo "blah blah blah")|}

  ; scribble "@foo{blah blah blah}" => [%sexp "foo" "blah blah blah"]

  ; scribble "@foo{blah blah blah}" => List [Sexp "foo"; Sexp "blah blah blah"]
*)
