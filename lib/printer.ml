type t = {
  buffer : Buffer.t;
  mutable indent_spaces : int;
  mutable columns : int;
  mutable printed_newline : bool;
  fail_on_overflow : bool;
  mutable bullets : Proof_bullet.t list list;
}

exception Exceeded_column_limit

let tab_size = 2
let columns_limit = 80

(* {{The doc} https://v2.ocaml.org/api/Buffer.html} says to allocate 16 buffers
   if unsure. *)
let create () =
  {
    buffer = Buffer.create 16;
    indent_spaces = 0;
    columns = 0;
    printed_newline = false;
    fail_on_overflow = false;
    bullets = [ [] ];
  }

let sequence xs printer = List.iter (fun x -> x printer) xs
let map_sequence f xs = sequence (List.map f xs)

let indents_for_bullets t =
  let num_bullets = function
    | Proof_bullet.Dash n | Proof_bullet.Plus n | Proof_bullet.Star n -> n
  in
  let indents_for_bullets_in_single_block bullets =
    (* +1 for the space after a bullet. *)
    List.fold_left (fun acc b -> acc + num_bullets b + tab_size + 1) 0 bullets
  in
  List.fold_left
    (fun acc b -> acc + indents_for_bullets_in_single_block b)
    0 t.bullets
  + (tab_size * (List.length t.bullets - 1))

let calculate_indent t = t.indent_spaces + indents_for_bullets t

let write s t =
  let string_to_push =
    if t.printed_newline then String.make (calculate_indent t) ' ' ^ s else s
  in
  let new_columns = t.columns + String.length string_to_push in
  if t.fail_on_overflow && new_columns > columns_limit then
    raise Exceeded_column_limit;

  Buffer.add_string t.buffer string_to_push;
  t.printed_newline <- false;
  t.columns <- new_columns

let newline t =
  if t.fail_on_overflow then raise Exceeded_column_limit;
  Buffer.add_char t.buffer '\n';
  t.printed_newline <- true;
  t.columns <- 0

module Str = struct
  let space = write " "
  let dot = write "."

  let blankline t =
    newline t;
    newline t
end

let start_subproof t = t.bullets <- [] :: t.bullets

let end_subproof t =
  match t.bullets with
  | [] -> failwith "end_subproof: empty list"
  | _ :: tail -> t.bullets <- tail

let increase_indent t = t.indent_spaces <- t.indent_spaces + tab_size

let decrease_indent t =
  t.indent_spaces <- t.indent_spaces - tab_size;
  if t.indent_spaces < 0 then
    failwith ("indent_spaces<0 : " ^ Buffer.contents t.buffer)

let indented f = sequence [ increase_indent; f; decrease_indent ]

let ( |=> ) hd p t =
  hd t;
  let l = t.indent_spaces in
  t.indent_spaces <- t.columns - indents_for_bullets t;
  p t;
  t.indent_spaces <- l

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
  match t.bullets with
  | [] -> failwith "bullet_appears: empty list"
  | h :: tail -> t.bullets <- update_bullet h :: tail

let clear_bullets t =
  match t.bullets with
  | [] -> failwith "clear_bullets: empty list"
  | _ :: tail -> t.bullets <- [] :: tail

module Wrap = struct
  let wrap before after f t = sequence [ write before |=> f; write after ] t
  let parens = wrap "(" ")"
  let braces = wrap "{" "}"
  let brackets = wrap "[" "]"
  let doublequoted = wrap "\"" "\""
end

module Lineup = struct
  let with_seps ~sep xs =
    sequence
      (List.mapi
         (fun i x -> match i with 0 -> x | _ -> sequence [ sep; x ])
         xs)

  let map_with_seps ~sep f xs = with_seps ~sep (List.map f xs)
  let map_commad f = map_with_seps ~sep:(write ", ") f
  let spaced = with_seps ~sep:Str.space
  let map_spaced f = map_with_seps ~sep:Str.space f
  let map_lined f = map_with_seps ~sep:newline f
  let map_bard f = map_with_seps ~sep:(write " | ") f
end

let copy_printer_by_value t =
  let buffer = Buffer.create 16 in
  let () = Buffer.add_string buffer (Buffer.contents t.buffer) in
  {
    buffer;
    indent_spaces = t.indent_spaces;
    columns = t.columns;
    printed_newline = t.printed_newline;
    fail_on_overflow = t.fail_on_overflow;
    bullets = t.bullets;
  }

let can_pp_oneline f printer =
  let printer_backup =
    { (copy_printer_by_value printer) with fail_on_overflow = true }
  in

  try
    f printer_backup;
    true
  with Exceeded_column_limit -> false

let try_pp_oneline f printer =
  if can_pp_oneline f printer then
    let () = f printer in
    true
  else false

let ( <-|> ) horizontal vertical printer =
  if try_pp_oneline horizontal printer then () else vertical printer

let contents t = Buffer.contents t.buffer
