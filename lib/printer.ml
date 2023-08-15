type t = {
  buffer : Buffer.t;
  mutable indent_spaces : int;
  mutable printed_newline : bool;
  mutable bullets : Proof_bullet.t list;
}

let tab_size = 2

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () =
  {
    buffer = Buffer.create 16;
    indent_spaces = 0;
    printed_newline = false;
    bullets = [];
  }

let calculate_indent t =
  (* 2 for the bullet and a space after it. *)
  t.indent_spaces + ((tab_size + 2) * List.length t.bullets)

let write s t =
  if t.printed_newline then
    String.make (calculate_indent t) ' ' |> Buffer.add_string t.buffer;
  Buffer.add_string t.buffer s;
  t.printed_newline <- false

let space = write " "

let newline t =
  Buffer.add_char t.buffer '\n';
  t.printed_newline <- true

let blankline t =
  newline t;
  newline t

let increase_indent t = t.indent_spaces <- t.indent_spaces + tab_size
let decrease_indent t = t.indent_spaces <- t.indent_spaces - tab_size

let write_before_indent s t =
  t.indent_spaces <- t.indent_spaces - String.length s;
  write s t;
  t.indent_spaces <- t.indent_spaces + String.length s

let bullet_appears bullet t =
  let rec update_bullet = function
    | [] -> [ bullet ]
    | h :: _ when h = bullet -> [ bullet ]
    | h :: t -> h :: update_bullet t
  in
  t.bullets <- update_bullet (List.rev t.bullets) |> List.rev

let clear_bullets t = t.bullets <- []

let parens f t =
  write "(" t;
  f ();
  write ")" t

let brackets f t =
  write "[" t;
  f ();
  write "]" t

let with_seps ~sep f xs =
  List.iteri
    (fun i x ->
      match i with
      | 0 -> f x
      | _ ->
          sep ();
          f x)
    xs

let commad f xs printer = with_seps ~sep:(fun () -> write ", " printer) f xs
let spaced f xs printer = with_seps ~sep:(fun () -> space printer) f xs
let bard f xs printer = with_seps ~sep:(fun () -> write " | " printer) f xs
let contents t = Buffer.contents t.buffer
