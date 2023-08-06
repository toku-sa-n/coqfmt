type t = {
  buffer : Buffer.t;
  mutable indent_level : int;
  mutable printed_newline : bool;
}

let tab_size = 2

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () =
  { buffer = Buffer.create 16; indent_level = 0; printed_newline = false }

let write t s =
  if t.printed_newline then
    String.make (tab_size * t.indent_level) ' ' |> Buffer.add_string t.buffer;
  Buffer.add_string t.buffer s;
  t.printed_newline <- false

let space t = write t " "

let newline t =
  Buffer.add_char t.buffer '\n';
  t.printed_newline <- true

let increase_indent t = t.indent_level <- t.indent_level + 1
let decrease_indent t = t.indent_level <- t.indent_level - 1

let parens t f =
  write t "(";
  f ();
  write t ")"

let contents t = Buffer.contents t.buffer
