type t = {
  buffer : Buffer.t;
  mutable indent_level : int;
  mutable printed_newline : bool;
  mutable inside_theorem : bool;
}

let tab_size = 2

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () =
  {
    buffer = Buffer.create 16;
    indent_level = 0;
    printed_newline = false;
    inside_theorem = false;
  }

let write t s =
  if t.printed_newline then
    String.make (tab_size * t.indent_level) ' ' |> Buffer.add_string t.buffer;
  Buffer.add_string t.buffer s;
  t.printed_newline <- false

let space t = write t " "

let newline t =
  Buffer.add_char t.buffer '\n';
  t.printed_newline <- true

let blankline t =
  newline t;
  newline t

let increase_indent t = t.indent_level <- t.indent_level + 1
let decrease_indent t = t.indent_level <- t.indent_level - 1

let inside_theorem t f =
  t.inside_theorem <- true;
  f ();
  t.inside_theorem <- false

let is_inside_theorem t = t.inside_theorem

let parens t f =
  write t "(";
  f ();
  write t ")"

let with_seps ~sep f xs =
  List.iteri
    (fun i x ->
      match i with
      | 0 -> f x
      | _ ->
          sep ();
          f x)
    xs

let commad printer = with_seps ~sep:(fun () -> write printer ", ")
let spaced printer = with_seps ~sep:(fun () -> space printer)
let contents t = Buffer.contents t.buffer
