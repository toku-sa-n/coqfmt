type t = {
  buffer : Buffer.t;
  mutable indent_level : int;
  mutable printed_newline : bool;
  mutable bullets : Proof_bullet.t list;
}

let tab_size = 2

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () =
  {
    buffer = Buffer.create 16;
    indent_level = 0;
    printed_newline = false;
    bullets = [];
  }

let calculate_indent t =
  match List.length t.bullets with
  | 0 -> t.indent_level * tab_size
  | n -> (t.indent_level * tab_size) + tab_size + ((tab_size + 2) * (n - 1))
(* 2 for the bullet and a space after it. *)

let write t s =
  if t.printed_newline then
    String.make (calculate_indent t) ' ' |> Buffer.add_string t.buffer;
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

let bullet_appears t bullet =
  let rec update_bullet = function
    | [] -> [ bullet ]
    | h :: _ when h = bullet -> [ bullet ]
    | h :: t -> h :: update_bullet t
  in
  t.bullets <- update_bullet (List.rev t.bullets) |> List.rev

let clear_bullets t = t.bullets <- []

let parens t f =
  write t "(";
  f ();
  write t ")"

let brackets t f =
  write t "[";
  f ();
  write t "]"

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
let bard printer = with_seps ~sep:(fun () -> write printer " | ")
let contents t = Buffer.contents t.buffer
