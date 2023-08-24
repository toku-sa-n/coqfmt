type t = {
  buffer : Buffer.t;
  mutable indent_spaces : int;
  mutable columns : int;
  mutable printed_newline : bool;
  (* TODO: Rename this as it is used not only for detecting the columns limit,
     but also the appearances of newlines. *)
  hard_fail_on_exceeding_column_limit : bool;
  mutable bullets : Proof_bullet.t list;
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
    hard_fail_on_exceeding_column_limit = false;
    bullets = [];
  }

let sequence xs printer = List.iter (fun x -> x printer) xs
let map_sequence f xs = sequence (List.map f xs)

let calculate_indent t =
  (* 2 for the bullet and a space after it. *)
  t.indent_spaces + ((tab_size + 2) * List.length t.bullets)

let write s t =
  let string_to_push =
    if t.printed_newline then String.make (calculate_indent t) ' ' ^ s else s
  in
  let new_columns = t.columns + String.length string_to_push in
  if t.hard_fail_on_exceeding_column_limit && new_columns > columns_limit then
    raise Exceeded_column_limit;

  Buffer.add_string t.buffer string_to_push;
  t.printed_newline <- false;
  t.columns <- new_columns

let space = write " "

let newline t =
  if t.hard_fail_on_exceeding_column_limit then raise Exceeded_column_limit;
  Buffer.add_char t.buffer '\n';
  t.printed_newline <- true;
  t.columns <- 0

let blankline t =
  newline t;
  newline t

let increase_indent t = t.indent_spaces <- t.indent_spaces + tab_size

let decrease_indent t =
  t.indent_spaces <- t.indent_spaces - tab_size;
  if t.indent_spaces < 0 then
    failwith ("indent_spaces<0 : " ^ Buffer.contents t.buffer)

let indented f = sequence [ increase_indent; f; decrease_indent ]

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
let parens f = sequence [ write "("; f; write ")" ]
let brackets f = sequence [ write "["; f; write "]" ]

let with_seps ~sep f xs =
  sequence
    (List.mapi
       (fun i x -> match i with 0 -> f x | _ -> sequence [ sep; f x ])
       xs)

let map_commad f = with_seps ~sep:(write ", ") f
let map_spaced f = with_seps ~sep:space f
let map_bard f = with_seps ~sep:(write " | ") f

let copy_printer_by_value t =
  let buffer = Buffer.create 16 in
  let () = Buffer.add_string buffer (Buffer.contents t.buffer) in
  {
    buffer;
    indent_spaces = t.indent_spaces;
    columns = t.columns;
    printed_newline = t.printed_newline;
    hard_fail_on_exceeding_column_limit = t.hard_fail_on_exceeding_column_limit;
    bullets = t.bullets;
  }

let overwrite_printer src dst =
  Buffer.clear dst.buffer;
  Buffer.add_string dst.buffer (Buffer.contents src.buffer);
  dst.indent_spaces <- src.indent_spaces;
  dst.columns <- src.columns;
  dst.printed_newline <- src.printed_newline;
  dst.bullets <- src.bullets

let ( <-|> ) horizontal vertical printer =
  let hor_printer =
    {
      (copy_printer_by_value printer) with
      hard_fail_on_exceeding_column_limit = true;
    }
  in

  try
    horizontal hor_printer;
    overwrite_printer hor_printer printer
  with Exceeded_column_limit ->
    if printer.hard_fail_on_exceeding_column_limit then
      raise Exceeded_column_limit
    else vertical printer

let contents t = Buffer.contents t.buffer
